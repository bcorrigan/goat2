package org.goat.module;

import org.goat.core.Module;
import org.goat.core.Message;


/**
 * Copyright (c) 2004 Robot Slave Enterprise Solutions
 *
 * @title More
 *
 *  @author encontrado
 *
 * @version 1.0
 */
public class More extends Module {

    public int messageType() {
        return WANT_COMMAND_MESSAGES;
    }
    public String[] getCommands() {
        return new String[]{"more", "moar"};
    }

    public More() {
    }

    public void processPrivateMessage(Message m) {
        processChannelMessage(m) ;
    }

    public void processChannelMessage(Message m) {
        if (m.getModText().trim().equals("")) {
            if (m.hasNextPage())
                m.createNextPage().send();
            //else
            //  m.reply("No more :(") ;
        }
    }
}
