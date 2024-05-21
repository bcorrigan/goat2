package org.goat.module;
/*
Copyright Paul James Mutton, 2001-2004, http://www.jibble.org/

This file is part of ReminderBot.

This software is dual-licensed, allowing you to choose between the GNU
General Public License (GPL) and the www.jibble.org Commercial License.
Since the GPL may be too restrictive for use in a proprietary application,
a commercial license is also provided. Full license information can be
found at http://www.jibble.org/licenses/

$Author: pjm2 $
$Id: ReminderBot.java,v 1.3 2004/05/29 19:44:30 pjm2 Exp $

*/

import java.util.*;
        import java.util.concurrent.ExecutorService;
import java.util.regex.*;
        import java.io.*;

import com.zoho.hawking.HawkingTimeParser;
import com.zoho.hawking.language.english.model.DateGroup;
import com.zoho.hawking.language.english.model.ParserOutput;
import org.goat.Goat;
import org.goat.core.*;
        import org.goat.util.Reminder;
import com.zoho.hawking.datetimeparser.configuration.HawkingConfiguration;
import com.zoho.hawking.language.english.model.DatesFound;

public class Remind extends org.goat.core.Module {

    private static final String REMINDER_FILE = "resources/reminders";
    private static org.goat.core.Users users;	//all the users
    private ReminderTimer timer;
    private LinkedList<Reminder> reminders = new LinkedList<Reminder>();
    private ExecutorService pool = Goat.modController.getPool();
    HawkingConfiguration hawkingConfiguration;
    HawkingTimeParser parser;
    Pattern taskPattern = Pattern.compile("^.*?\\s+to\\s+(.*?)$");

    public Remind() {
        loadReminders();
        //setName(name);
        //setAutoNickChange(true);

        timer = new ReminderTimer(this);
        pool.execute(timer);
        users = org.goat.Goat.getUsers() ;
        this.hawkingConfiguration = new HawkingConfiguration();
        try {
            hawkingConfiguration.setFiscalYearStart(4);
            hawkingConfiguration.setFiscalYearEnd(3);
            hawkingConfiguration.setTimeZone("Europe/London");
        } catch(Exception e) {
            e.printStackTrace();
            System.out.println("Fatal error in Remind module:" + e.getMessage());
            System.exit(1);
        }
        parser = new HawkingTimeParser();
    }

    public void processChannelMessage(Message m) {
        DatesFound dates = parser.parse(m.getText(), new Date(), this.hawkingConfiguration, "eng");
        List<ParserOutput> po = dates.getParserOutputs();
        if (!po.isEmpty()) {
            long due = po.getFirst().getDateRange().getStart().getMillis();
            Matcher matcher = taskPattern.matcher(m.getModText());
            if(matcher.matches()) {
                String task = matcher.group(1);
                Reminder reminder = new Reminder(m.getChatId(), getName(m), m.getSender(), task, System.currentTimeMillis(), due);
                String name = getName(m);
                String replyName = null;
                if(name.equals(m.getSender()))
                    replyName = "you";
                else
                    replyName = name;
                GregorianCalendar cal = new GregorianCalendar(TimeZone.getDefault());
                cal.setTimeInMillis( reminder.getDueTime() );

                String date = String.format(Locale.UK, "%1$td/%1$tm/%1$ty %1$tR", cal);
                m.reply(m.getSender() + ": Okay, I'll remind " + replyName + " about that on " + date);
                reminders.add(reminder);
                timer.interrupt();
            } else {
                m.reply("Sorry, I could work out the date, but not what you want to be reminded of!");
            }
        } else {
            m.reply("Sorry, I can't work out any kind of date or time from that!");
        }
    }

    private String getName(Message m) {
        String[] words = m.getModText().split(" ");
        if(words[0].equals("me"))
            return m.getSender();
        return words[0];
    }

    public void processPrivateMessage(Message m) {
        processChannelMessage(m);
    }

    public String[] getCommands() {
        return new String[]{"remind"};
    }

    private double getPeriod(String periods, String regex) throws NumberFormatException {
        Pattern pattern = Pattern.compile("^.*?([\\d\\.]+)\\s*(?i:(" + regex + ")).*$");
        Matcher m = pattern.matcher(periods);
        m = pattern.matcher(periods);
        if (m.matches()) {
            double d = Double.parseDouble(m.group(1));
            if (d < 0 || d > 1e6) {
                throw new NumberFormatException("Number too large or negative (" + d + ")");
            }
            return d;
        }
        return 0;
    }

    private class ReminderTimer implements Runnable {

        private Remind secretary;
        ReminderTimer(Remind reminder) {
            secretary = reminder;
        }
        private Thread myThread = null;

        public synchronized void run() {
            myThread = Thread.currentThread();
            boolean running = true;
            while (true) {

                // If the list is empty, wait until something gets added.
                if (secretary.reminders.isEmpty()) {
                    try {
                        wait();
                    }
                    catch (InterruptedException e) {
                        // Do nothing.
                    }
                }

                Reminder reminder = secretary.reminders.getFirst();
                long delay = reminder.getDueTime() - System.currentTimeMillis();
                if (delay > 0) {
                    try {
                        wait(delay);
                    }
                    catch (InterruptedException e) {
                        // A new Reminder was added. Sort the list.
                        Collections.sort(secretary.reminders);
                        secretary.saveReminders();
                    }
                }
                else {
                    String replyName = reminder.getReminder();

                    if(replyName==null || replyName.equals(reminder.getNick()))
                        replyName = "You";
                    Message m = new Message(reminder.getChannel(), reminder.getNick() + ": " + replyName + " asked me to remind you " + reminder.getMessage(), false, replyName );
                    m.send();
                    //Message.createPrivmsg(reminder.getChannel(), reminder.getNick() + ": " + replyName + " asked me to remind you " + reminder.getMessage());
                    secretary.reminders.removeFirst();
                    secretary.saveReminders();
                }
            }
        }

        public synchronized void interrupt() {
            if(myThread != null)
                myThread.interrupt();
        }
    }

    private void saveReminders() {
        try {
            ObjectOutputStream out = new ObjectOutputStream(new FileOutputStream(new File(REMINDER_FILE)));
            out.writeObject(reminders);
            out.flush();
            out.close();
        }
        catch (Exception e) {
            // If it doesn't work, no great loss!
        }
    }

    private void loadReminders() {
        try {
            ObjectInputStream in = new ObjectInputStream(new FileInputStream(new File(REMINDER_FILE)));
            Object ob = in.readObject();
            if(ob instanceof LinkedList) {
                LinkedList<?> ll = (LinkedList<?>) ob;
                for(Object item: ll)
                    if(item instanceof Reminder)
                        reminders.add((Reminder) item);
            }
            in.close();
        }
        catch (Exception e) {
            // If it doesn't work, no great loss!
        }
    }



}
