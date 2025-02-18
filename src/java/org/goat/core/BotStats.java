package org.goat.core;

import java.io.IOException;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import org.goat.util.StringUtil;

import java.nio.charset.Charset;
import java.nio.file.FileSystem;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;

import java.util.HashSet;
import java.util.Set;
import java.io.File;

import com.google.common.jimfs.Configuration;
import com.google.common.jimfs.Jimfs;

/**
 * <p>Contains lots of info about the bot and its environment.
 *
 * @author <p><b>? Barry Corrigan</b> All Rights Reserved.</p>
 * @version <p>Date: 18-Dec-2003</p>
 */
public class BotStats {
    private static BotStats instance;
    private String token;

    //in memory file system for those APIs that insist on passing Files
    private FileSystem fs = Jimfs.newFileSystem(Configuration.unix());

    public Path memoryFile(String name, byte[] bytes) {
        Path path = fs.getPath(name);
        try {
            Files.write(path, bytes);
        } catch (IOException e) {
            System.err.println("IO Exception writing to memory - are we out of memory?");
            System.exit(1);
        }
        return path;
    }
    /**
     * Made this a CopyOnWriteArrayList to hopefully avoid the concurrent modification exceptions.
     * In theory, it should avoid the problems we have had, but will be much, much slower for write operations
     * But who cares! we only really write to the list at startup. Big whoop.
     */
    private List<Module> modules = new CopyOnWriteArrayList<Module>();

    private BotStats() {
        //pass
    }

    public static BotStats getInstance() {
        if (instance == null)
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

    /*
     * Where to find the config file.
     */
    private String CONFIG_FILE = "config/goatrc";

    /**
     * The bot's name.
     */
    private String botname;
    /**
     * The channels we are in.
     */
    private List<String> channels = new CopyOnWriteArrayList<String>();
    /**
     * The authenticated owner of the bot.
     */
    private String owner;
    /**
     * Client version.
     */
    private String version;
    /**
     * The name of the server we are connected to.
     */
    private String servername;
    private String clientName;

    private String hostmask;

    private Set<String> commands = new HashSet<String>();

    /**
     * Set to true in unit test context
     */
    private boolean testing = false;

    /**
     * The charset the bot is currently using
     */
    private Charset charset = Charset.forName("UTF-8");

    /**
     * List of names to ignore.
     */
    private List<String> ignoreNames = new CopyOnWriteArrayList<String>();

    public void setToken(String token) {
        this.token = token;
    }

    protected String getToken() {
        return token;
    }

    public String getBotname() {
        return botname;
    }

    public void setBotname(String botname) {
        this.botname = botname;
    }

    public String getOwner() {
        return owner;
    }

    public void setOwner(String owner) {
        this.owner = owner;
    }

    public String getVersion() {
        return version;
    }

    public void setVersion(String version) {
        this.version = version;
    }

    public String getServername() {
        return servername;
    }

    public void setServername(String servername) {
        this.servername = servername;
    }

    public String getClientName() {
        return clientName;
    }

    public void setClientName(String clientName) {
        this.clientName = clientName;
    }

    public String getHostmask() {
        return hostmask;
    }

    public void setHostmask(String hostmask) {
        this.hostmask = hostmask;
    }

    public Set<String> getCommands() {
        return commands;
    }

    public void setCommands(Set<String> commands) {
        this.commands = commands;
    }

    public List<Module> getModules() {
        return modules;
    }

    public void rebuildCommands() {
        BotStats.getInstance().setCommands(new HashSet<String>());
        for (Module mod : getModules()) {
            addCommands(mod.getCommands());
        }
    }

    public boolean isLoadedCommand(String s) {
        return StringUtil.stringInArray(s, getCommands().toArray(new String[0]));
    }

    public void addCommands(String[] commands) {
        BotStats.getInstance().getCommands().addAll(Arrays.asList(commands));
    }

    public void setModules(List<Module> modules) {
        this.modules = modules;
    }

    public void addModule(Module module) {
        modules.add(module);
    }

    public void removeModule(Module module) {
        modules.remove(module);

        rebuildCommands();
    }

    /**
     * @return All the module names of all loaded modules
     */
    public String[] getModuleNames() {
        Module mod;
        String[] modNames = new String[modules.size()];
        for (int i = 0; i < modules.size(); i++) {
            mod = modules.get(i);
            modNames[i] = mod.moduleName;
        }
        return modNames;
    }

    public boolean isTesting() {
        return testing;
    }

    public void setTesting(boolean testing) {
        this.testing = testing;
    }

    public String getCONFIG_FILE() {
        return CONFIG_FILE;
    }

    public synchronized String[] getChannels() {
        Object[] ob = channels.toArray();
        String[] chans = new String[ob.length];
        for (int i = 0; i < ob.length; i++) {
            chans[i] = (String) ob[i];
        }
        return chans;
    }

    public synchronized void addChannel(String chan) {
        channels.add(chan);
    }

    public synchronized void removeChannel(String chan) {
        channels.remove(chan);
    }

    public synchronized boolean containsChannel(String chan) {
        return channels.contains(chan);
    }

    public boolean isValidChannelName(String chan) {
        return (chan.startsWith("#") || chan.startsWith("+") || chan.startsWith("&")) && !(chan.matches(":") || chan.matches(",") || chan.matches("\u0007") || chan.matches(" "));
    }

    /**
     * Get the currently used charset from here
     *
     * @return
     */
    public Charset getCharset() {
        return charset;
    }

    /**
     * Tracking the ignoreNames list.
     */
    public synchronized String[] getIgnoreNames() {
        Object[] ob = ignoreNames.toArray();
        String[] names = new String[ob.length];
        for (int i = 0; i < ob.length; i++) {
            names[i] = (String) ob[i];
        }
        return names;
    }

    public synchronized void addIgnoreName(String name) {
        ignoreNames.add(name);
    }

    public synchronized void removeIgnoreName(String name) {
        ignoreNames.remove(name);
    }

    public synchronized boolean containsIgnoreName(String name) {
        return ignoreNames.contains(name);
    }


}

