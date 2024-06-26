package org.goat.core;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.File;
import java.util.ArrayList;
import java.util.Set;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

import javax.script.ScriptException;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.Invocable;

import org.reflections.Reflections;
/*
import org.python.core.Py;
import org.python.core.PySystemState;
import goat.module.JSR223Module;
 */


/**
 * Loads and unloads Modules. Provides methods allowing you
 * to find out what Modules are loaded, to control the Modules, and
 * to return them.
 * @version Date: 17-Dec-2003
 * @author
 *
 */
public class ModuleController  {

    private ExecutorService pool = Executors.newCachedThreadPool();

    public ExecutorService getPool() {
        return pool;
    }

    //private ArrayList<Module> loadedModules = new ArrayList<Module>();

    private ArrayList<Class<? extends Module>> allModules = new ArrayList<Class<? extends Module>>() ;
    //private ArrayList<String> allCommands = new ArrayList<String>() ;
    private ArrayList<File> allScriptModules = new ArrayList<File>();



    private BotStats bot = BotStats.getInstance();

    public ModuleController() {
        System.out.println("Starting construction of module controller..");
        buildAllModulesList();
        //buildScriptModulesList();
        System.out.println("constructed module controller?");
    }

    /**
     * Loads a module.
     *
     * @param moduleName
     * @return
     * @throws IllegalAccessException
     * @throws InstantiationException
     * @throws ClassNotFoundException
     * @throws IOException
     * @throws ScriptException
     * @throws NoSuchMethodException
     * @throws NoClassDefFoundError
     * @throws ClassCastException
     */
    public Module load(String moduleName)
            throws ClassNotFoundException, IllegalAccessException, InstantiationException, IOException, NoSuchMethodException, ScriptException {
        if(moduleName.contains(".")) {
            //if the name has a dot, assume a script
            System.out.print("Loading Module " + moduleName + " ... ");
            File file = new File(scriptDir(moduleName) + File.separatorChar + moduleName);
            BufferedReader bis = new BufferedReader(  new FileReader(file));
            String line;
            String script="";
            while((line=bis.readLine())!=null) {
                script+=line+"\n";
            }
            String extension = moduleName.replaceAll(".*\\.", "");
            ScriptEngine engine = new ScriptEngineManager().getEngineByExtension(extension);
            engine.eval(script);
            Invocable inv = (Invocable) engine;
            Module mod;
            Object moduleCandidate = inv.invokeFunction("getInstance");
            //if(moduleCandidate instanceof Module) {
                mod = (Module) moduleCandidate;
            //} else {
            //    mod = new JSR223Module(extension,script);
            //} this can be restored if we want scripting back
            mod.moduleName=moduleName;
            bootModule(mod);
            return mod;
        } else {
            moduleName = "org.goat.module." + moduleName;
            return load(Class.forName(moduleName));
        }

    }

    public Module loadInAllChannels(String moduleName)
            throws ClassNotFoundException, IllegalAccessException, InstantiationException, IOException, NoSuchMethodException, ScriptException {
        Module ret = load(moduleName);
        if (null == ret) // wasn't loaded
            ret = getLoaded(moduleName);
        ret.inAllChannels = true;
        return ret;
    }

    public Module load(Class<?> modClass) throws IllegalAccessException, InstantiationException {
        if (null == modClass)
            return null;
        System.out.print("Loading Module " + modClass.getName() + " ... ");

        Module module = null;
        // return null if module is already loaded
        if(null != getLoaded(modClass))
            return null;

        module = (Module) modClass.newInstance();

        bootModule(module);
        module.moduleName=modClass.getName().replace("org.goat.module.", "");
        return module;
    }

    private void bootModule(Module module) {
        if(null == module)
            return;
        pool.execute(module);
        while(! module.isRunning()) {// wait to make sure module is running before adding it to loaded module list
            try {
                Thread.sleep(5);  // short wait, it shouldn't take too long for the thread pool to start the module
            } catch (InterruptedException ie) {}
        }
        System.out.print("running ... ");

        bot.addModule(module);

        bot.addCommands(module.getCommands());

        System.out.println("loaded.");
    }

    public Module loadInAllChannels(Class<?> modClass)
            throws IllegalAccessException, InstantiationException {
        Module ret = load(modClass);
        if(null != ret)
            ret.inAllChannels = true;
        return ret;
    }

    public boolean unload(String moduleName) {
        Module mod = getLoaded(moduleName);
        if (null == mod)
            return false ;
        else {
            bot.removeModule(mod) ;
            mod.stopDispatcher();
        }

        return true ;
    }

    public Module getLoaded(int i) {
        return bot.getModules().get(i);
    }

    public Module getLoaded(Class<?> modClass) {
        Iterator<Module> it = bot.getModules().listIterator();
        Module ret = null;
        if(!modClass.equals(org.goat.core.Module.class) && !modClass.equals(Object.class))
            while(it.hasNext()) {
                Module mod = it.next();
                if(modClass.isInstance(mod))
                    ret = mod;
            }
        return ret;
    }

    public Module getLoaded(String modName) {
        List<Module> mods = bot.getModules();
        for(Module mod: mods) {
            if(mod.moduleName.equals(modName)) {
                return mod;
            }
        }
        return null;
    }

    /**
     * Build the list of all public classes in package goat.module.
     *
     * This gets sort of ugly.  It sure would be nice if the java reflection API
     * could give us a list of all classes in a package, wouldn't it?
     */
    private void buildAllModulesList() {
        if(bot.isTesting())
            return;

        Reflections reflections = new Reflections("org.goat");
        Set<Class<? extends Module>> allModules =
                reflections.getSubTypesOf(Module.class);

        ArrayList<String> tempList = new ArrayList<String>();
        for (Class<? extends Module> modClass : allModules) {
            tempList.add(modClass.getName());
        }
        Collections.sort(tempList);
        System.out.println("Available Modules: ") ;
        for (String modName : tempList) {
            System.out.println("   " + modName);
        }
        System.out.println() ;
    }

    public final String pyModDir = "src" + File.separatorChar + "main" + File.separatorChar + "python";

    /**
     * Looks in script dir(s) and assumes any files in there are modules.
     */
    private void buildScriptModulesList() {
        // FIXME: only python mods dir atm, should include anything that
        //   scriptDir() might return
        String path = pyModDir;
        File folder = new File(path);
        File[] listOfFiles = folder.listFiles();
        System.out.println("Available script modules:");
        for(File scriptModuleFile : listOfFiles) {
            System.out.println("   " +scriptModuleFile.getName());
            allScriptModules.add(scriptModuleFile);
        }
    }

    public List<Class<? extends Module>> getAllModules() {
        return allModules;
    }

    public String scriptDir(String name) {
        String ret = "scripts";
        if (name.endsWith(".py"))
            ret = pyModDir;
        return ret;
    }
}
