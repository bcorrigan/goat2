package org.goat.core;

import org.telegram.telegrambots.meta.api.methods.send.SendMessage;
import org.telegram.telegrambots.meta.exceptions.TelegramApiException;
import org.telegram.telegrambots.meta.generics.TelegramClient;
import org.goat.Goat;

import java.util.StringTokenizer;
import java.util.concurrent.LinkedBlockingQueue;

/**
 * This will let us slightly abstract / wrap interaction with telegram and simplify code in modules
 *
 * It also will make it easier to copy/adapt code from old goat..
 */
public class Message {
    //true if this message was sent by the *owner* of the bot
    private boolean isAuthorised;

    /**
     * The outqueue instance for sending messages
     */
    private static LinkedBlockingQueue<Message> outqueue = Goat.outqueue;

    private TelegramClient tclient;

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
    }

    public Message(Long chatId, String text, Boolean isPrivate, String sender) {
        this.chatId = chatId;
        this.text = text;
        this.isPrivate = isPrivate;
        this.sender = sender;

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
                while (st.hasMoreTokens())
                    this.modText = st.nextToken() + ' ';//TODO all this String concatenation in loops is nae use, need to replace with StringBuffer. But StringBuilder comes with jdk1.5, so will just wait till it is widespread
            }
        } else {
            setDirectlyAddressed(false);
            this.modCommand = firstWord;
            while (st.hasMoreTokens())
                this.modText = st.nextToken() + ' ';
        }
    }


    private void setDirectlyAddressed(boolean direct) {
        this.directlyAddressed = direct;
    }

    //Create a reply and send it immediately.
    public void reply(String msg) {
        outqueue.add(new Message(chatId, msg, isPrivate, sender));
    }

    public void send() {
        outqueue.add(this);
    }

    public SendMessage getSendMessage() {
        //create an object that contains the information to send back the message
        SendMessage sm =  new SendMessage(this.chatId.toString(), text);
        sm.setParseMode("html");
        return sm;
    }

    public boolean isPrivate() {
        return isPrivate;
    }

    public String getSender() {
        return sender;
    }

}
