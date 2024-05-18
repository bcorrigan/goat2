package org.goat.core;

import java.net.*;
import java.nio.charset.Charset;
import java.io.*;
import java.sql.SQLException;
import java.util.Date;
import java.util.List;
import java.util.concurrent.LinkedBlockingQueue;
import org.goat.Goat;
import org.telegram.telegrambots.meta.generics.TelegramClient;
import org.telegram.telegrambots.client.okhttp.OkHttpTelegramClient;
import org.telegram.telegrambots.longpolling.TelegramBotsLongPollingApplication;
import org.telegram.telegrambots.longpolling.util.LongPollingSingleThreadUpdateConsumer;
import org.telegram.telegrambots.meta.api.methods.send.SendMessage;
import org.telegram.telegrambots.meta.api.objects.Update;
//import org.telegram.telegrambots.meta.api.objects.message.Message;
import org.telegram.telegrambots.meta.exceptions.TelegramApiException;

/**
 * Maintains the connection with the server. A seperate thread, this.
 *
 * @author <p><b>Barry Corrigan</b>
 * @version <p>Date: 14.12.2003</p>
 */

public class ServerConnection extends Thread {

    public boolean debug = false;

    private static LinkedBlockingQueue<Message> inqueue = Goat.inqueue; //Queue of messages FROM the server
    private static LinkedBlockingQueue<Message> outqueue = Goat.outqueue; //Queue of messages TO the server

    private InputHandler ih;
    private OutputHandler oh;
    private final TelegramClient telegramClient;
    private volatile boolean connected = false;

    /**
     * Connects us to Telegram..
     */
    public ServerConnection() {
        telegramClient = new OkHttpTelegramClient(BotStats.getInstance().getToken());
        reconnect();
    }

    private void connect() throws IOException {
        ih = new InputHandler();
        oh = new OutputHandler();

        oh.start();
        ih.start();
    }

    private void reconnect() {
        connected = false;
        while (true) {
            try {
                connect();
                return;
            } catch (UnknownHostException uhe) {
                System.out.println("Hmmn unknown host, will wait 400 seconds then try connecting again.. ");
            } catch (IOException ioe) {
                System.out.println("IOException, waiting 400 secs then retry. ");
            } catch (Exception e) {
                System.err.println("Unexpected exception while trying reconnect() :");
                e.printStackTrace();
            }

            try {
                sleep(400000);
            } catch (InterruptedException e) {
                System.err.println("Interrupted from sleep between reconnect attempts :");
                e.printStackTrace();
            } catch (Exception e) {
                System.err.println("Unexpected exception while sleeping between reconnects :");
                e.printStackTrace();
            }

        }
    }

    class InputHandler extends Thread implements LongPollingSingleThreadUpdateConsumer {
        private volatile boolean keeprunning = true;

        public void run() {
            try (TelegramBotsLongPollingApplication botsApplication = new TelegramBotsLongPollingApplication()) {
                botsApplication.registerBot(BotStats.getInstance().getToken(), this);
                Thread.currentThread().join();
            } catch (Exception e) {
                System.err.println("Error registering bot, msg: " + e);
                e.printStackTrace();
                System.exit(2);
            }
            System.out.println("New inputHandler created");
        }

        void disconnect() {
            keeprunning = false;
        }

        public void consume(Update update) {
                try {
                    Message m;

                    if (update.hasEditedMessage()) {
                        m = new Message(update.getEditedMessage());
                    } else if (update.hasMessage()) {
                        m = new Message(update.getMessage());
                    } else if (update.hasChannelPost()) {
                        m = new Message(update.getChannelPost());
                    } else {
                        System.out.println("Unknown update type!: " + update.toString());

                        return;
                    }

                    inqueue.add(m); //add to inqueue
                    if (debug)
                        System.out.println(m.toString());

                } catch (Exception e) {
                    System.err.println("Unexpected exception in InputHandler :" );
                    e.printStackTrace();
                }
            }

    public void consume(List<Update> updates) {
        LongPollingSingleThreadUpdateConsumer.super.consume(updates);
    }
    }

    class OutputHandler extends Thread {
        private volatile boolean keeprunning;

        void disconnect() {
            keeprunning = false;
        }

        public OutputHandler() {
            setName("Output Handler (client -> server)");
            keeprunning = true;
        }

        public void run() {
            while (keeprunning) {
                try {
                    Message m = outqueue.take();
                    if(!keeprunning)
                        break;

                    try {
                        telegramClient.execute(m.getSendMessage());
                    } catch (TelegramApiException e) {
                        //TODO what is error handling?
                        System.err.println("Some sort of error with telegram:" + e.getMessage());
                        e.printStackTrace();
                        Thread.sleep(300000); //lets try sleeping it off
                    }
                } catch (InterruptedException e) {
                } catch (Exception e) {
                    System.err.println("unexpected exception in OuputHandler");
                    e.printStackTrace();
                }
            }
        }
    }
}
