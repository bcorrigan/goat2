package org.goat;

import org.goat.core.*;
import static org.goat.util.Passwords.*;

import clojure.java.api.Clojure;
import clojure.lang.IFn;

import java.io.*;
import java.net.URL;
import java.util.Locale;


public class Goat {
    private static boolean showhelp;
    private static boolean testMode;
    public static String[] argv = {""};

    public static void main(String[] args) {
        argv=args;
        new Goat() ;
        System.out.println("Shouldn't get here?");
        System.exit(0) ;
    }

    public Goat() {
        Locale.setDefault(Locale.UK);  // goat is UKian, damn it.
        setDefaultStats();
        parseArgs(argv);
        if (showhelp) {
            showHelp();
            System.exit(0);
        }

        // Initialize Clojure module system first
        try {
            IFn require = Clojure.var("clojure.core", "require");
            require.invoke(Clojure.read("org.goat.core.init"));

            IFn init = Clojure.var("org.goat.core.init", "init!");
            init.invoke();
        } catch (Exception e) {
            System.err.println("Failed to initialize Clojure module system:");
            e.printStackTrace();
        }

        // Start connection (Telegram or CLI)
        if (testMode) {
            // CLI test mode - use stdin/stdout
            System.out.println("Starting in CLI test mode...");
            // TODO: Implement CLI connection using Clojure
            new CLIConnection();
        } else {
            // Normal mode - connect to Telegram
            System.out.print("Connecting to telegram... ");
            try {
                IFn require = Clojure.var("clojure.core", "require");
                require.invoke(Clojure.read("org.goat.core.connection"));

                IFn startConnection = Clojure.var("org.goat.core.connection", "start-connection");
                startConnection.invoke();
                System.out.println("We appear to be connected.\n");
            } catch (Exception e) {
                System.err.println("Failed to start Telegram connection:");
                e.printStackTrace();
                System.exit(1);
            }
        }

        // Old Java module loading (commented out - now using pure Clojure modules)
        // loadDefaultModules(modController);
        // try {
        //     Thread.sleep(100);   //lets give the logon a chance to progress before adding messages to queues
        // } catch (InterruptedException e) {
        //     e.printStackTrace();
        // }
        // new MessageDispatcher(modController);

        // Keep the main thread alive (core.async go-loops are daemon threads)
        try {
            Thread.currentThread().join();
        } catch (InterruptedException e) {
            System.out.println("Bot shutting down...");
        }
    }

    private static void parseArgs(String[] args) {
        int state = 0;

        for (String arg : args) {
            switch (state) {
                //start case
                case 0:
                    if (arg.equals("-name")) {
                        state = 1;
                    } else if (arg.equals("-channel")) {
                        state = 2;
                    } else if (arg.equals("-host")) {
                        state = 3;
                    } else if (arg.equals("-help")) {
                        showhelp = true;
                    } else if (arg.equals("-test")) {
                        testMode = true;
                    } else {
                        System.out.println("Illegal argument.");
                        showhelp = true;
                    }
                    break;

                case 1:
                    BotStats.getInstance().setBotname(arg);
                    state = 0;
                    break;

                case 2:
                    BotStats.getInstance().addChannel(arg);
                    state = 0;
                    break;

                case 3:
                    BotStats.getInstance().setServername(arg);
                    state = 0;
                    break;
            }
        }

        //if we are still waiting for an argument
        if (state != 0) {
            System.out.println("Missing argument.");
            showhelp = true;
        }
    }

    private static void showHelp() {
        System.out.println("Usage: java Goat [-name <name>][-host <host>][-channel <channel>][-test]");
        System.out.println();
        System.out.println("Options:");
        System.out.println("  -name <name>         Changes the bot's default name [default: goat]");
        System.out.println("  -channel <#channel>  Changes the bot's default channel [default: #jism]");
        System.out.println("  -host <host>         Sets which host to connect to [default: irc.slashnet.org]");
        System.out.println("  -test                Run in CLI test mode (stdin/stdout instead of Telegram)");
    }


    private void setDefaultStats() {
        // syntax is ugly here because we're accessing a scala companion object
        //BuildInfo$ buildInfo = goat.BuildInfo$.MODULE$;

        BotStats bs = BotStats.getInstance();
        //bs.setVersion(buildInfo.version() + "-r" + buildInfo.gitRevision());
        String nick = getPassword("nick");
        bs.setBotname(nick);
        bs.setClientName(nick);
        bs.setOwner(nick);
        bs.setToken(getPassword("telegram.token"));
    }

    public static Properties getProps() {
        return getPropsFromFile(GOAT_PROPS_FILE) ;
    }

}
