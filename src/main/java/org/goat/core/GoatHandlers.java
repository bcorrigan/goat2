package org.goat.core;

import lombok.extern.slf4j.Slf4j;
import org.telegram.telegrambots.client.okhttp.OkHttpTelegramClient;
import org.telegram.telegrambots.longpolling.TelegramBotsLongPollingApplication;
import org.telegram.telegrambots.longpolling.util.LongPollingSingleThreadUpdateConsumer;
import org.telegram.telegrambots.meta.api.methods.send.SendMessage;
import org.telegram.telegrambots.meta.api.objects.Update;
//import org.telegram.telegrambots.meta.api.objects.message.Message;
import org.telegram.telegrambots.meta.exceptions.TelegramApiException;
import org.telegram.telegrambots.meta.generics.TelegramClient;

import java.util.List;

@Slf4j
public class GoatHandlers implements LongPollingSingleThreadUpdateConsumer {

    private final TelegramClient telegramClient;

    public GoatHandlers(String botToken) {
        telegramClient = new OkHttpTelegramClient(botToken);
    }

    @Override
    public void consume(List<Update> updates) {
        LongPollingSingleThreadUpdateConsumer.super.consume(updates);
        //for (Update update : updates) {
        //    consume(update);
        //}
    }

    @Override
    public void consume(Update update) {
        System.out.println("Woohoo, got:" + update.getMessage().getText());

        Message msg = new Message(update.getMessage(), telegramClient);

        //create an object that contains the information to send back the message
        SendMessage sendMessageRequest = new SendMessage(update.getMessage().getChatId().toString(), "you said: " + update.getMessage().getText());
        try {
            telegramClient.execute(sendMessageRequest); //at the end, so some magic and send the message ;)
        } catch (TelegramApiException e) {
            //do some error handling
        }//end catch()
    }

    public static void main(String[] args) {
        try (TelegramBotsLongPollingApplication botsApplication = new TelegramBotsLongPollingApplication()) {
            botsApplication.registerBot(BotConfig.GOAT_TOKEN, new GoatHandlers(BotConfig.GOAT_TOKEN));
            Thread.currentThread().join();
        } catch (Exception e) {
            log.error("Error registering bot", e);
        }
    }
}
