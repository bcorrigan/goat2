package org.goat.core;

import org.telegram.telegrambots.meta.api.methods.send.SendMessage;
import org.telegram.telegrambots.meta.exceptions.TelegramApiException;
import org.telegram.telegrambots.meta.generics.TelegramClient;

import java.util.StringTokenizer;

/**
 * This will let us slightly abstract / wrap interaction with telegram and simplify code in modules
 *
 * It also will make it easier to copy/adapt code from old goat..
 */
public class Message {
    //true if this message was sent by the *owner* of the bot
    private boolean isAuthorised;

    private org.telegram.telegrambots.meta.api.objects.message.Message tmsg;

    private TelegramClient tclient;

    //"command" that modules may be interested in
    private String modCommand;

    //all the rest after the modCommand... (aka modTrailing)
    private String modText;

    //the raw text (aka trailing)
    private String text;

    //who sent this message?
    private String sender;

    //true if sent "privately" ie 1-1
    private boolean isPrivate;

    //the name of the chat room?
    private String chatname;

    //true if user "directly addressed" bot. For example, said "goat, blah blah blah".
    private boolean directlyAddressed;

    public Message(org.telegram.telegrambots.meta.api.objects.message.Message tmsg, TelegramClient tclient) {
        this.tmsg = tmsg;
        this.tclient = tclient;
        this.text = tmsg.getText();
        this.isPrivate = tmsg.isUserMessage(); //TODO this is a guess

        StringTokenizer st = new StringTokenizer(text);
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
        //create an object that contains the information to send back the message
        SendMessage sendMessageRequest = new SendMessage(this.tmsg.getChatId().toString(), msg);
        try {
            tclient.execute(sendMessageRequest);
        } catch (TelegramApiException e) {
            //TODO what is error handling?
        }
    }

    public boolean isPrivate() {
        return isPrivate;
    }

}
