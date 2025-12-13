package org.goat.core;

import clojure.java.api.Clojure;
import clojure.lang.IFn;

import java.io.*;

/**
 * CLI-based connection for local testing.
 * Reads input from stdin and writes output to stdout instead of connecting to Telegram.
 *
 * @author bc
 */
public class CLIConnection extends Thread {

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
     * Reads from stdin and creates message maps via Clojure
     */
    class InputHandler extends Thread {
        private volatile boolean keeprunning = true;

        public void run() {
            setName("CLI Input Handler (stdin -> incoming-chan)");
            BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
            String line;

            try {
                // Load Clojure namespaces
                IFn require = Clojure.var("clojure.core", "require");
                require.invoke(Clojure.read("org.goat.core.message-parse"));
                require.invoke(Clojure.read("org.goat.core.channels"));

                IFn createMessage = Clojure.var("org.goat.core.message-parse", "create-message");
                IFn putIncoming = Clojure.var("org.goat.core.channels", "put-incoming!");

                while (keeprunning && (line = reader.readLine()) != null) {
                    if (line.trim().isEmpty()) {
                        continue;
                    }

                    // Create a test message map
                    // chatId: 123456L, text: user input, private?: true, sender: "TestUser"
                    Object msg = createMessage.invoke(
                        Clojure.read(":chat-id"), 123456L,
                        Clojure.read(":text"), line,
                        Clojure.read(":sender"), "TestUser",
                        Clojure.read(":private?"), true
                    );

                    // Put on incoming channel
                    putIncoming.invoke(msg);
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
     * Reads from outgoing channel and writes to stdout
     */
    class OutputHandler extends Thread {
        private volatile boolean keeprunning = true;

        public OutputHandler() {
            setName("CLI Output Handler (outgoing-chan -> stdout)");
        }

        public void run() {
            try {
                // Load Clojure namespaces
                IFn require = Clojure.var("clojure.core", "require");
                require.invoke(Clojure.read("org.goat.core.channels"));
                require.invoke(Clojure.read("org.goat.core.message"));

                IFn takeOutgoing = Clojure.var("org.goat.core.channels", "take-outgoing!");
                IFn getText = Clojure.var("org.goat.core.message", "text");
                IFn hasImage = Clojure.var("org.goat.core.message", "has-image?");

                while (keeprunning) {
                    // Take message from outgoing channel (blocking)
                    Object msg = takeOutgoing.invoke();
                    if (!keeprunning || msg == null)
                        break;

                    // Handle text messages
                    Object textObj = getText.invoke(msg);
                    if (textObj != null) {
                        System.out.println(textObj.toString());
                    }

                    // Handle image messages
                    Object hasImageResult = hasImage.invoke(msg);
                    if (hasImageResult != null && (Boolean) hasImageResult) {
                        System.out.println("[IMAGE: Cannot display images in CLI mode]");
                    }
                }
            } catch (Exception e) {
                System.err.println("Unexpected exception in CLI OutputHandler: " + e.getMessage());
                e.printStackTrace();
            }
        }

        void disconnect() {
            keeprunning = false;
        }
    }
}
