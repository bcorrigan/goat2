package org.goat.core;

public class Constants {


    // Formatting

    /**
     * Bold text.
     */
    public static final String BOLD = "<b>";
    public static final String END_BOLD = "</b>";

    public static final String PRE = "<pre>";
    public static final String END_PRE = "</pre>";
    /**
     * Removes all previously applied color and formatting attributes.
     */
    public static final String NORMAL = "";
    /**
     * Underlined text.
     */
    public static final String UNDERLINE = "<u>";
    public static final String END_UNDERLINE = "</u>";
    /**
     * Reversed text (may be rendered as italic text in some clients).
     */
    public static final String REVERSE = "";

    // Colors



    // Assorted useful UTC-16 glyphs
    public static final String LATIN_CAPITAL_LETTER_B_WITH_STROKE = "\u0243";
    public static final String BUTTCOIN = LATIN_CAPITAL_LETTER_B_WITH_STROKE;

    public static final String WHITE_SUN_WITH_RAYS = "\u263C";
    public static final String BLACK_SUN_WITH_RAYS = "\u2600";
    public static final String SNOWFLAKE = "\u2744";
    public static final String UMBRELLA = "\u2602";
    public static final String UMBRELLA_WITH_RAIN_DROPS = "\u2614";
    public static final String CLOUD = "\u2601";
    public static final String SUN_BEHIND_CLOUD = "\u26C5";
    public static final String SNOWMAN_WITHOUT_SNOW = "\u26C4";
    public static final String BLACK_SNOWMAN = "\u26C7";
    public static final String THUNDER_CLOUD_AND_RAIN = "\u26C8";

    // Emoji.

    public static final String GOATJI = new String(Character.toChars(128016));
    public static final String LOUDLY_CRYING_FACE = new String(Character.toChars(128557));
    public static final String PILE_OF_POO = new String(Character.toChars(128169));
    public static final String PUBLIC_ADDRESS_LOUDSPEAKER = new String(Character.toChars(128226));
    public static final String CHRISTMAS_TREE = new String(Character.toChars(127876));
    public static final String BROKEN_HEART = new String(Character.toChars(128148));

    public static final String CLOSED_UMBRELLA = new String(Character.toChars(127746));
    public static final String CYCLONE = new String(Character.toChars(127744));
    public static final String FOGGY = new String(Character.toChars(127745));
    public static final String DASH_SYMBOL = new String(Character.toChars(128168));

    // Emoonji
    public static final String NEW_MOON_SYMBOL = new String(Character.toChars(127761));
    public static final String WAXING_CRESCENT_MOON_SYMBOL = new String(Character.toChars(127762));
    public static final String FIRST_QUARTER_MOON_SYMBOL = new String(Character.toChars(127763));
    public static final String WAXING_GIBBOUS_MOON_SYMBOL = new String(Character.toChars(127764));
    public static final String FULL_MOON_SYMBOL = new String(Character.toChars(127765));
    public static final String WANING_GIBBOUS_MOON_SYMBOL = new String(Character.toChars(127766));
    public static final String LAST_QUARTER_MOON_SYMBOL = new String(Character.toChars(127767));
    public static final String WANING_CRESCENT_MOON_SYMBOL = new String(Character.toChars(127768));



    //time related
    public static final long SECOND = 1000;
    public static final long MINUTE = SECOND * 60;
    public static final long HOUR = MINUTE * 60;
    public static final long DAY = HOUR * 24;
    public static final long YEAR = (long) ((double) DAY * 365.25);
    public static final long MONTH = YEAR / 12;
}
