package org.goat.core;

import org.telegram.telegrambots.meta.generics.TelegramClient;
import org.telegram.telegrambots.meta.api.methods.send.SendMessage;
import org.telegram.telegrambots.meta.api.methods.send.SendPhoto;
import org.telegram.telegrambots.meta.api.methods.send.SendDocument;
import org.telegram.telegrambots.meta.api.methods.GetFile;
import org.telegram.telegrambots.meta.api.objects.InputFile;
import org.telegram.telegrambots.meta.api.objects.File;
import org.goat.Goat;
import org.goat.util.Pager;
import org.telegram.telegrambots.meta.exceptions.TelegramApiException;

import javax.imageio.ImageIO;
import java.awt.image.RenderedImage;
import java.io.*;
import java.nio.file.Path;
import java.io.ByteArrayInputStream;
import java.util.StringTokenizer;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.LinkedBlockingQueue;

/**
 * This will let us slightly abstract / wrap interaction with telegram and simplify code in modules
 *
 * It also will make it easier to copy/adapt code from old goat..
 */
public class Message {
    //true if this message was sent by the *owner* of the bot
    private boolean isAuthorised;

    //don't think we need an enum for "type" of message. ie a telegram msg could have text AND various other media
    private boolean hasImage=false;
    private boolean hasDocument=false;

    public boolean hasImage() {
        return hasImage;
    }

    public boolean hasText() {
        return hasText;
    }

    //thought about - have a Message interface, then implement TextMessage, PictureMessage etcetc
    //But I think in telegram you can have messages which are all of these at once so maybe
    //just add processPictureMsg to Module and call it a day?
    private boolean hasText;

    private static ConcurrentHashMap<Long, Pager> pagerCache = new ConcurrentHashMap<>() ;
    /**
     * The outqueue instance for sending messages
     */
    private static LinkedBlockingQueue<Message> outqueue = Goat.outqueue;

    private TelegramClient tclient; // nullable in CLI mode

    public String getModCommand() {
        return modCommand;
    }

    //"command" that modules may be interested in
    private String modCommand;

    public String getModText() {
        return modText;
    }

    //all the rest after the modCommand... (aka modTrailing)
    private String modText;

    public String getText() {
        return text;
    }

    //the raw text (aka trailing)
    private String text;

    public Long getChatId() {
        return chatId;
    }

    private Long chatId;

    private RenderedImage image;

    private byte[] imageBytes;

    private byte[] documentBytes;

    private String documentFilename;

    //who sent this message?
    private String sender;

    //true if sent "privately" ie 1-1
    private boolean isPrivate;

    public String getChatname() {
        return chatname;
    }

    //the name of the chat room?
    private String chatname;

    //true if user "directly addressed" bot. For example, said "goat, blah blah blah".
    private boolean directlyAddressed;

    public Message(org.telegram.telegrambots.meta.api.objects.message.Message tmsg) {
        this(tmsg.getChatId(), tmsg.getText(), tmsg.isUserMessage(), tmsg.getFrom().getFirstName());
		if (tmsg.isGroupMessage()) {
			chatname = tmsg.getChat().getTitle();
		}
    }

    //private - only used to construct outgoing messages for now
    private Message(Long chatId, RenderedImage image, Boolean isPrivate, String sender) {
        this.chatId = chatId;
        this.image = image;
        this.isPrivate = isPrivate;
        this.sender = sender;
        hasImage=true;
    }

    //private - only used to construct outgoing document messages
    private Message(Long chatId, byte[] documentBytes, String filename, Boolean isPrivate, String sender) {
        this.chatId = chatId;
        this.documentBytes = documentBytes;
        this.documentFilename = filename;
        this.isPrivate = isPrivate;
        this.sender = sender;
        hasDocument=true;
    }

    public Message(Long chatId, String text, Boolean isPrivate, String sender) {
        this.chatId = chatId;
        this.text = text;
        this.isPrivate = isPrivate;
        this.sender = sender;

        StringBuilder modTextSB = new StringBuilder();

        if(text==null) {
            hasText=false;
            return;
        }
        hasText=true;

        StringTokenizer st = new StringTokenizer(text); //NPE if text is empty?
        String firstWord = "";
        if (st.hasMoreTokens()) {
            firstWord = st.nextToken() ;
        }
        if ((!firstWord.toLowerCase().matches(BotStats.getInstance().getBotName().toLowerCase() + "\\w+"))
                && firstWord.toLowerCase().matches(BotStats.getInstance().getBotName().toLowerCase() + "\\W*")) {
            setDirectlyAddressed(true);
            if (st.hasMoreTokens()) {
                this.modCommand = st.nextToken();
                while (st.hasMoreTokens()) {
                    modTextSB.append(st.nextToken()).append(' ');
                    //this.modText = modText + st.nextToken() + ' ';//TODO all this String concatenation in loops is nae use, need to replace with StringBuffer. But StringBuilder comes with jdk1.5, so will just wait till it is widespread
                }
            } else {
                this.modCommand = firstWord;
            }
        } else {
            setDirectlyAddressed(false);
            this.modCommand = firstWord;
            while (st.hasMoreTokens())
                modTextSB.append(st.nextToken()).append(' ');
                //this.modText = modText + st.nextToken() + ' ';
        }
        this.modText = modTextSB.toString().trim();
    }


    private void setDirectlyAddressed(boolean direct) {
        this.directlyAddressed = direct;
    }

    //Create a reply and send it immediately.
    public void reply(String msg) {
        outqueue.add(createPagedReply(msg));
    }

    public void replyWithImage(RenderedImage image) {
        outqueue.add(new Message(chatId, image, isPrivate, sender));
    }

    public void replyWithDocument(byte[] bytes, String filename) {
        outqueue.add(new Message(chatId, bytes, filename, isPrivate, sender));
    }

    public boolean hasDocument() {
        return hasDocument;
    }

    public byte[] getDocumentBytes() {
        return documentBytes;
    }

    public String getDocumentFilename() {
        return documentFilename;
    }

    public void setIncomingDocument(String filename, byte[] bytes) {
        this.documentFilename = filename;
        this.documentBytes = bytes;
        this.hasDocument = true;
    }

    public void send() {
        outqueue.add(this);
    }

    public SendMessage getSendMessage() {
        //create an object that contains the information to send back the message
        SendMessage sm = new SendMessage(this.chatId.toString(), text);
        sm.setParseMode("html");
        return sm;
    }

    public SendPhoto getSendPhoto() {
        //convert RenderedImage to "in memory file" & return as SendPhoto
        var baos = new ByteArrayOutputStream();

        try {
            ImageIO.write(image, "png", baos);
            //imageFile = BotStats.getInstance().memoryFile("/" + chatId.toString() + "-" + UUID.randomUUID() + ".png", baos.toByteArray());
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        InputStream istr = new ByteArrayInputStream(baos.toByteArray());
        //System.out.println("image:" + imageFile.getFileName());
        return new SendPhoto(this.chatId.toString(), new InputFile(istr, "image.png"));
    }

    public SendDocument getSendDocument() {
        //convert byte array to "in memory file" & return as SendDocument
        InputStream istr = new ByteArrayInputStream(documentBytes);
        return new SendDocument(this.chatId.toString(), new InputFile(istr, documentFilename));
    }

    /*
    public void sendImageUploadingAFile(String filePath, String chatId) {
    // Create send method
    SendPhoto sendPhotoRequest = new SendPhoto();
    // Set destination chat id
    sendPhotoRequest.setChatId(chatId);
    // Set the photo file as a new photo (You can also use InputStream with a constructor overload)
    sendPhotoRequest.setPhoto(new InputFile(new File(filePath)));
    try {
        // Execute the method
        telegramClient.execute(sendPhotoRequest);
    } catch (TelegramApiException e) {
        e.printStackTrace();
    }
}
     */

    public boolean isPrivate() {
        return isPrivate;
    }

    public String getSender() {
        return sender;
    }

    //paging stuff

    /**
     * Creates a new paged reply, using createReply(), and initializes the pager cache with the supplied string
     *
     * @param text The text to be paged and sent
     *
     * @return a message containing the first chunk of paged text, which the caller will most likely want to send()
     */
    public Message createPagedReply(String text) {
        return createPagedPrivmsg(chatId, text);
    }

    public Message createPagedPrivmsg(long to, String message) {
        Message ret;
        if(Pager.shouldPaginate(message)) {
            Pager pager = new Pager(message) ;
            pagerCache.put(to, pager) ;
            ret = new Message(to, pager.getNext(), isPrivate, sender) ;
        } else {
            ret = new Message(chatId, message, isPrivate, sender);
        }
        return ret;
    }

    public boolean hasNextPage() {
        return hasNextPage(chatId);
    }

    public boolean hasNextPage(Long key) {
        boolean ret = false;
        if (pagerCache.containsKey(key) ) {
            Pager pager = pagerCache.get(key) ;
            if (pager.isEmpty())
                pagerCache.remove(key);
            else
                ret = true;
        }
        return ret ;
    }

    /**
     * returns a reply message via createReply containing the next page of text, if any, from the pager cache for the current channel/nick (ie, "params")
     *
     * @return aforesaid message, if there is more text in the buffer, else an empty message.
     */
    public Message createNextPage() {
        Message ret;
        if (hasNextPage() )
            ret = new Message(chatId, nextPage(chatId), isPrivate, sender) ;
        else {
            ret = new Message(chatId, "", isPrivate, sender);
        }

        return ret;
    }

    public String nextPage(Long key) {
        String ret = "";
        if (hasNextPage(key)) {
            Pager pager = pagerCache.get(key);
            ret = pager.getNext();
            if (pager.isEmpty())
                pagerCache.remove(key);
        }
        return ret;
    }
}
