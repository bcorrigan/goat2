package org.goat.util;
/*
Copyright Paul James Mutton, 2001-2004, http://www.jibble.org/

This file is part of ReminderBot.

This software is dual-licensed, allowing you to choose between the GNU
General Public License (GPL) and the www.jibble.org Commercial License.
Since the GPL may be too restrictive for use in a proprietary application,
a commercial license is also provided. Full license information can be
found at http://www.jibble.org/licenses/

$Author: pjm2 $
$Id: Reminder.java,v 1.1 2004/05/12 22:01:43 pjm2 Exp $

*/


import java.io.Serializable;

public class Reminder implements Serializable, Comparable {

    /**
     * eclipse moans without this.
     */
    private static final long serialVersionUID = 1L;

    public Reminder(Long channel, String nick, String reminder, String message, long setTime, long dueTime) {
        this.channel = channel;
        this.nick = nick;
        this.message = message;
        this.setTime = setTime;
        this.dueTime = dueTime;
        this.reminder = reminder;
    }

    public long getChannel() {
        return channel;
    }

    public String getNick() {
        return nick;
    }

    public String getReminder() {
        return reminder;
    }

    public String getMessage() {
        return message;
    }

    public long getSetTime() {
        return setTime;
    }

    public long getDueTime() {
        return dueTime;
    }

    public int compareTo(Object o) {
        if (o instanceof Reminder) {
            Reminder other = (Reminder) o;
            if (dueTime < other.dueTime) {
                return -1;
            }
            else if (dueTime > other.dueTime) {
                return 1;
            }
        }
        return 0;
    }

    private long channel;
    private String nick;
    private String message;
    private String reminder;
    private long setTime;
    private long dueTime;

}
