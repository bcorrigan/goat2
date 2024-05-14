package org.goat.core;

public class BotStats {
    private static BotStats instance;

    private BotStats() {
        //pass
    }

    public static BotStats getInstance() {
        if(instance==null)
            instance = new BotStats();
        return instance;
    }

    public String getBotName() {
        return botName;
    }

    public void setBotName(String botName) {
        this.botName = botName;
    }

    private String botName = "goat"; //TODO config

}
