package org.goat.core;

import org.goat.Goat;

import java.io.*;
import java.util.concurrent.LinkedBlockingQueue;

/**
 * CLI-based connection for local testing.
 * Reads input from stdin and writes output to stdout instead of connecting to Telegram.
 *
 * @author bc
 */
public class CLIConnection extends Thread {

    private static LinkedBlockingQueue<Message> inqueue = Goat.inqueue;
    private static LinkedBlockingQueue<Message> outqueue = Goat.outqueue;

    private InputHandler ih;
    private OutputHandler oh;

    public CLIConnection() {
        System.out.println("Starting CLI test mode...");
        System.out.println("Type commands (e.g., 'roll 3d6' or 'goat, mem')");
        System.out.println("Press Ctrl+C to exit\n");
        connect();
    }

    private void connect() {
        ih = new InputHandler();
        oh = new OutputHandler();

        oh.start();
        ih.start();
    }

    /**
     * Reads from stdin and creates Message objects
     */
    class InputHandler extends Thread {
        private volatile boolean keeprunning = true;

        public void run() {
            setName("CLI Input Handler (stdin -> inqueue)");
            BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
            String line;

            try {
                while (keeprunning && (line = reader.readLine()) != null) {
                    if (line.trim().isEmpty()) {
                        continue;
                    }

                    // Create a test message
                    // chatId: 123456L, text: user input, isPrivate: true, sender: "TestUser"
                    Message m = new Message(123456L, line, true, "TestUser");
                    inqueue.add(m);
                }
            } catch (IOException e) {
                System.err.println("Error reading from stdin: " + e.getMessage());
                e.printStackTrace();
            }
        }

        void disconnect() {
            keeprunning = false;
        }
    }

    /**
     * Reads from outqueue and writes to stdout
     */
    class OutputHandler extends Thread {
        private volatile boolean keeprunning = true;

        public OutputHandler() {
            setName("CLI Output Handler (outqueue -> stdout)");
        }

        public void run() {
            while (keeprunning) {
                try {
                    Message m = outqueue.take();
                    if (!keeprunning)
                        break;

                    // Handle text messages
                    if (m.hasText()) {
                        System.out.println(m.getText());
                    }

                    // Handle image messages
                    if (m.hasImage()) {
                        System.out.println("[IMAGE: Cannot display images in CLI mode]");
                    }

                } catch (InterruptedException e) {
                    // Thread interrupted, continue
                } catch (Exception e) {
                    System.err.println("Unexpected exception in CLI OutputHandler: " + e.getMessage());
                    e.printStackTrace();
                }
            }
        }

        void disconnect() {
            keeprunning = false;
        }
    }
}
