package org.goat.module;

import java.util.concurrent.Callable;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.goat.Goat;
import org.goat.core.Module;
import org.goat.core.Message;
import org.goat.jcalc.Calculator;
import org.goat.jcalc.CalculatorException;

/**
 * A piece of glue code to tie goat to the calculator code stolen (JCalc?)
 *
 * @author bc
 */
public class Calc extends Module {

    Calculator calc = new Calculator();

    public Calc() {

        // set up some prevention of obvious qpting
        //  ... less necessary now that we've added a couple of interruptability knobs to jcalc
        //      in addition to these admittedly lame computation limits

        //calc.setLimitFactorial(15342L);
        //calc.setLimitUpperPower(100);
        //calc.setLimitLowerPower(-100);
    }

    public void processPrivateMessage(Message m) {
        processChannelMessage(m);
    }

    public void processChannelMessage(Message m) {

        ExecutorService pool = Goat.modController.getPool();
        Cogitator brain = new Cogitator(m);
        Future<String> future = pool.submit(brain);
        String reply;
        try {
            reply = future.get(2000, TimeUnit.MILLISECONDS);
        } catch (CancellationException ce) {
            reply = "I've gone ahead and cancelled that computation for you.  Asshat.";
        } catch (TimeoutException te) {

            // important:  when you get a timeout exception via Future.get(), the underlying
            //   calculation has not stopped.  This means you might be able to DOS the goat
            //   with this, so don't tell qpt.
            boolean status = future.cancel(true);	// this doesn't actually stop the underlying calculation, either,
            // unless you've taken pains to make sure it's interruptable.
            // which we have.
            StackTraceElement[] trace = brain.getThread().getStackTrace();
            for (StackTraceElement element : trace) {
                System.out.println(element.toString());
            }
            if(status) {
                reply = "I'm not thinking that hard, wanker.";
            } else {
                reply = "Why are you forcing me to think so hard???";
            }
        } catch (ExecutionException ee) {
            reply = ee.getCause().getLocalizedMessage();
        } catch (InterruptedException ie) {
            reply = "I'm sorry, where were we before we were so rudely interrupted?";
        }
        if(reply.length() > 256)
            reply = reply.length() + " digits:  " + reply;
        m.reply(reply); //TODO make paged
    }

    /**
     * The thinkin' thread
     */
    private class Cogitator implements Callable<String> {

        public Thread getThread() {
            return thread;
        }

        private Thread thread;

        private Message target;

        public Cogitator(Message m) {
            target = m;
        }

        public String call() throws CalculatorException, InterruptedException {
            thread = Thread.currentThread();
            return calc.evaluate_equation(target.getModText());
        }
    }

    public String[] getCommands() {
        return new String[]{"calc"};
    }
}
