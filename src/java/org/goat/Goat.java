package org.goat;

import org.goat.core.*;
//import org.goat.module.Core;
//import org.goat.module.ModuleCommands;
import static org.goat.util.Passwords.*;

import java.io.*;
import java.net.URL;
import java.util.Locale;
import java.util.concurrent.LinkedBlockingQueue;


public class Goat {
    private static boolean showhelp;
    private static boolean testMode;
    public static LinkedBlockingQueue<Message> inqueue = new LinkedBlockingQueue<Message>();
    public static LinkedBlockingQueue<Message> outqueue = new LinkedBlockingQueue<Message>();
    public static ModuleController modController = new ModuleController() ;
    public static String[] argv = {""};
    public static ServerConnection sc;

    private static Users users = new Users() ;

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

        if (testMode) {
            // CLI test mode - use stdin/stdout
            new CLIConnection();
        } else {
            // Normal mode - connect to Telegram
            System.out.print("Connecting to telegram... ");
            sc = new ServerConnection(); //lets init the connection..
            System.out.println("We appear to be connected.\n");
        }

        loadDefaultModules(modController);
        try {
            Thread.sleep(100);   //lets give the logon a chance to progress before adding messages to queues
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        new MessageDispatcher(modController);
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

    private void loadDefaultModules(ModuleController modController) {
        Class<?>[] defaultModules = {
            org.goat.module.WordGame.class,
            org.goat.module.Remind.class,
            org.goat.module.CountDown.class,
            org.goat.module.Calc.class,
            org.goat.module.More.class,
            org.goat.module.DiceRoll.class,
            org.goat.module.Define.class,
            org.goat.module.Weather.class
            //CljTest.class
            /*goat.module.ModuleCommands.class,
              goat.module.NickServ.class,
              goat.module.Help.class,
              goat.module.Auth.class,
              goat.module.Core.class,
              goat.module.UserManagement.class,
              goat.module.ServerCommands.class*/
        } ;
        try {
            for(int i=0; i<defaultModules.length; i++)
                modController.loadInAllChannels(defaultModules[i]);
            modController.loadInAllChannels("Wordle");
            modController.loadInAllChannels("CoreCommands");
			modController.loadInAllChannels("Capture");
            /*ModuleCommands moduleCommands = (ModuleCommands) modController.getLoaded("ModuleCommands");
            moduleCommands.modControl = modController;
            moduleCommands.inAllChannels = true;*/
        } catch (Exception e) {
            e.printStackTrace();
        }
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

    public static Users getUsers() {
        if (null == users) {
            users = new Users() ;
            return users ;
        } else {
            return users;
        }
    }
}
