package org.goat.module;
/*
 * Dice Module: For all your 5d6 and 1d100 fun.
 */
import java.text.ParseException;

import org.goat.core.Constants;
import org.goat.core.Module;
import org.goat.core.Message;

import org.goat.dice.*;

/**
 * Dice throwing module.
 * @author bc
 */
public class DiceRoll extends Module {

    //kind of crap but at least goat.dice doesn't know about the rest of goat this way
    static {
        Dice.BOLD = Constants.BOLD;
        Dice.UNDERLINE = Constants.UNDERLINE;
        Dice.NORMAL = Constants.NORMAL;
        Dice.END_UNDERLINE = Constants.END_UNDERLINE;
        Dice.END_BOLD = Constants.END_BOLD;
    }

    public void processPrivateMessage(Message m) {
        if( m.getModCommand().equals("roll")) {
            String roll = m.getModText();
            try {
                if( Dice.estimateSize(roll)>100) {
                    m.reply("I'm not rolling that many dice, I'd be here all day!");
                    return;
                }
                Dice dice = new Dice(roll);
                dice.throwDice();
                m.reply(m.getSender() + ": " + dice);
            } catch(ParseException pe) {
                pe.printStackTrace();
                if( pe.getMessage().equals("Throw size too big"))
                    m.reply("It is so funny to make me try and throw more dice than exist in the universe.");
                else if ( pe.getMessage().equals("Dice size too big"))
                    m.reply("I'm not rolling a sphere, sorry.");
                else if( pe.getMessage().equals("Error parsing roll"))
                    m.reply("Sorry, I don't know how to do that.");
                else
                    m.reply("An unidentified error occurred with roll: " + pe.getMessage());
            }
        } else if( m.getModCommand().equals("toss") && ( m.getModText().equals("") || m.getModText().contains("coin")  )) {
            Die die = new Die(2);
            die.throwDie();
            if( die.getResult()==2 )
                m.reply("Heads.");
            else
                m.reply("Tails.");
        }
    }

    public void processChannelMessage(Message m) {
        processPrivateMessage(m) ;
    }

    public int messageType() {
        return WANT_COMMAND_MESSAGES;
    }

    public String[] getCommands() {
        return new String[]{"roll", "toss"};
    }
}

