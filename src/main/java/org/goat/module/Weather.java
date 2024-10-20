package org.goat.module;
/*
 * Created on 14-Aug-2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */

import static org.goat.core.Constants.*;

import org.goat.core.Message;
import org.goat.core.Module;
import org.goat.core.User;
import org.goat.core.Users;
import org.goat.core.KVStore;
import org.goat.util.PhaseOfMoon;
//import org.goat.util.WeatherStore;

import java.net.URISyntaxException;
import java.util.Calendar;
import java.util.ArrayList;
import static java.util.Collections.sort;
import static java.util.Collections.min;

import java.net.URL;
import java.net.HttpURLConnection;
import java.net.SocketTimeoutException;
import java.util.regex.* ;
        import java.util.Date ;
import java.text.SimpleDateFormat ;
import org.goat.suntimes.*;
        import java.util.GregorianCalendar ;
import java.util.TimeZone ;
import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.Serializable;

/**
 * @author bc
 *         <p/>
 *         Module that allows users to ask for weather reports (METAR data) by supplying a four letter ICAO code
 */
public class Weather extends Module {

    private static Users users;	//all the users of this weather module
    private String codes_url = "http://aviationweather.gov/static/adds/metars/stations.txt";
    //private WeatherStore wStore = new WeatherStore();

    public Weather() {
        users = org.goat.Goat.getUsers();
    }


    public boolean isThreadSafe() {
        return false;
    }

    /* (non-Javadoc)
     * @see goat.core.Module#processPrivateMessage(goat.core.Message)
     */
    public void processPrivateMessage(Message m) {
        processChannelMessage(m);
    }

    /* (non-Javadoc)
     * @see goat.core.Module#processChannelMessage(goat.core.Message)
     */
    public void processChannelMessage(Message m) {
        if (m.getModText().matches("\\s*")) {     //if just whitespace

            if (users.hasUser(m.getSender()) && ! users.getUser(m.getSender()).getWeatherStation().equals("")) {
                m.reply(getReport(m.getSender(), m.getModCommand(),
                        users.getUser(m.getSender()).getWeatherStation()));
                return;
            }

                m.reply("I don't know where you are, " + m.getSender() + ", perhaps you should tell me " +
                        "by looking at" + BOLD + " " + codes_url + " " + END_BOLD +
                        "and telling me where you are.");

        } else if (m.getModText().matches("\\s*[a-zA-Z0-9]{4}\\s*")) { //if 4 letter code is supplied
            String station = m.getModText().trim().toUpperCase() ;

            String report = getReport(m.getSender(), m.getModCommand(), station);
            //debug
            //System.out.println("report:" + report + ":");
            if (report.matches(".*[ (]" + station + "[).].*")) {
                User user = users.getOrCreateUser(m.getSender());
                if (! user.getWeatherStation().equals(station)) {
                    user.setWeatherStation(station);
                }
            }
            m.reply(report);
        }
    }

    private String getMetar(String username, String command, String station) {
        // System.out.println("getting metar");
        HttpURLConnection connection = null;
        BufferedReader in = null;
        station = station.toUpperCase() ;
        String response = "";
        try {
            URL url = new URL("https://tgftp.nws.noaa.gov/data/observations/metar/stations/" + station + ".TXT");
            connection = (HttpURLConnection) url.openConnection();
            connection.setConnectTimeout(5000);  //just five seconds, we can't hang around
            connection.connect();
            if (connection.getResponseCode() == HttpURLConnection.HTTP_NOT_FOUND) {
                return "That doesn't seem to be a valid location, " + username + ", sorry.  See " + codes_url ;
            }
            if (connection.getResponseCode() != HttpURLConnection.HTTP_OK) {
                return "Hmmmn. " + username + ", the NOAA weather server is giving me an HTTP Status-Code " + connection.getResponseCode() + ", sorry.";
            }
            in = new BufferedReader(new InputStreamReader(connection.getInputStream()));
            String inputLine = null ;
            while ((inputLine = in.readLine()) != null) {
                inputLine = inputLine.trim();
                if(!inputLine.equals("")) {
                    if (!response.equals("")) {
                        response = response + " " + inputLine;
                    } else {
                        response = inputLine;
                    }
                }
            }
        } catch  (SocketTimeoutException e) {
            response = "I got bored waiting for the METAR for " + station ;
        } catch (IOException e) {
            e.printStackTrace();
            response = "I had an I/O problem when trying to fetch the METAR for " + station;
        } finally {
            if(connection!=null) connection.disconnect();
            try {
                if(in!=null) in.close();
            } catch (IOException ioe) {
                System.out.println("Cannot close input stream");
                ioe.printStackTrace();
            }
        }
        return response;
    }

   private String getReport(String username, String command, String station) {
        if (command.toLowerCase().matches(".*(:?raw|metar).*"))
            return getMetar(username, command, station);
        HttpURLConnection connection = null;
        BufferedReader in = null;
        station = station.toUpperCase() ;
        try {
            URL url = new URL("https://tgftp.nws.noaa.gov/data/observations/metar/decoded/" + station + ".TXT");
            connection = (HttpURLConnection) url.openConnection();
            // incompatible with 1.4
            connection.setConnectTimeout(5000);  //just five seconds, we can't hang around
            connection.connect();
            if (connection.getResponseCode() == HttpURLConnection.HTTP_NOT_FOUND) {
                return "That doesn't seem to be a valid location, " + username + ", sorry.  See " + codes_url ;
            }
            if (connection.getResponseCode() != HttpURLConnection.HTTP_OK) {
                return "Hmmmn. " + username + ", the NOAA weather server is giving me an HTTP Status-Code " + connection.getResponseCode() + ", sorry.";
            }
            in = new BufferedReader(new InputStreamReader(connection.getInputStream()));
            String inputLine = null ;
            String response = "" ;
            String wind_direction = "";
            String wind_mph = "";
            //String precipitation = "";
            String wind_gust = "";
            String temp_f = "";
            String temp_c = "";
            String sky_conditions = "";
            String weather_type = "";
            String precipitation = "none";
            String humidity = "";
            Date report_date = null;
            long minutes_since_report = 0 ;
            // String report_timezone = "" ;
            int report_year = 0;
            int report_month = 0;
            int report_day = 0;
            int report_hour = 0;
            int report_minute = 0;
            double longitude = 0;
            double latitude = 0;
            while ((inputLine = in.readLine()) != null) {
                if (inputLine.startsWith("ob") || inputLine.startsWith("cycle") || inputLine.contains("Windchill"))
                    continue;
                inputLine = inputLine.replaceAll(":0", "");
                if (inputLine.matches(".*: .*") && inputLine.substring(0, 1).matches("[A-Z]")) {
                    inputLine = BOLD + inputLine.replaceAll(": ", ':' + END_BOLD + ' ');
                }
                // Might want to move these pattern compiles out of the while loop...

                // Wind
                Matcher m = Pattern.compile("^<b>Wind:</b> from the ([NSEW]+) \\(.*\\) at (\\d+) MPH \\(\\d+ KT\\)(?: gusting to (\\d+) MPH \\(\\d+ KT\\))*.*").matcher(inputLine) ;
                if (m.matches()) {
                    wind_direction = m.group(1) ;
                    wind_mph = m.group(2) ;
                    if (!(null ==  m.group(3))) {
                        wind_gust = m.group(3) ;
                    }
                }



                // Coordinates
                m = Pattern.compile(".* (\\d+)-(\\d+)(?:-\\d+)*([NS]) (\\d+)-(\\d+)(?:-\\d+)*([EW]).*").matcher(inputLine) ;
                if (m.matches()) {
                    //System.out.println("matched: " + m.group()) ;
                    latitude = Double.parseDouble(m.group(1)) + Double.parseDouble(m.group(2)) / 60L ;
                    if ((m.group(3) != null) && (m.group(3).equals("S"))) {
                        latitude = - latitude ;
                    }
                    longitude = Double.parseDouble(m.group(4)) + Double.parseDouble(m.group(5)) / 60L ;
                    if ((m.group(6) != null) && (m.group(6).equals("W"))) {
                        longitude = - longitude ;
                    }
                    //System.out.println("coordinates (lat/long): " + latitude + "/" + longitude) ;
                }

                // Sky conditions
                m = Pattern.compile("^<b>Sky conditions:</b> (.*)").matcher(inputLine) ;
                if (m.matches()) {
                    sky_conditions = m.group(1) ;
                }

                // Weather type
                m = Pattern.compile("^<b>Weather:</b> (.*)").matcher(inputLine) ;
                if (m.matches()) {
                    weather_type = m.group(1) ;
                }

                // Time
                m = Pattern.compile(".* ([A-Z]{3}) / ((\\d+)\\.(\\d+)\\.(\\d+) (\\d{2})(\\d{2}) UTC).*").matcher(inputLine) ;
                if (m.matches()) {
                    // By way of explanation:  the regexp should yield groups:
                    //  (1) local time zone as "ZZZ"
                    //		note: as far as I've seen, this is always EST --rs
                    //report_timezone = m.group(1) ;  //unused
                    //  (2) UTC date and time as "yyyy.MM.dd HHmm UTC"
                    //  (3) year as "yyyy"
                    report_year = Integer.parseInt(m.group(3));
                    //  (4) month as "MM"
                    report_month = Integer.parseInt(m.group(4));
                    //  (5) day as "dd"
                    report_day = Integer.parseInt(m.group(5));
                    report_hour =  Integer.parseInt(m.group(6));
                    report_minute =  Integer.parseInt(m.group(7));
                    SimpleDateFormat sdf = new SimpleDateFormat("yyyy.MM.dd HHmm zzz") ;
                    try {
                        report_date =  sdf.parse(m.group(2));
                        Date now = new Date();
                        minutes_since_report =  (now.getTime() - report_date.getTime()) / 1000L / 60L;
                    } catch(java.text.ParseException e) {
                        System.out.println("Date parse exception.");
                        e.printStackTrace();
                    }

                }

                // Temperature
                m = Pattern.compile("^<b>Temperature:</b> ([\\d.-]+) F \\(([\\d.-]+) C\\)").matcher(inputLine) ;
                if (m.matches()) {
                    temp_f = m.group(1) ;
                    temp_c = m.group(2) ;
                    inputLine = "<b>Temperature:</b> " + temp_c + " C";
                }

                m = Pattern.compile("^<b>Dew Point:</b> ([\\d.-]+) F \\(([\\d.-]+) C\\)").matcher(inputLine);
                if(m.matches()) {
                    String c = m.group(2);
                    inputLine = "<b>Dew Point:</b> " + c + " C";
                }

                // Precipitation
                m = Pattern.compile("^<b>Precipitation last hour:</b> (.*)").matcher(inputLine) ;
                if (m.matches()) {
                    //uncomment if we start using this again
                    precipitation = m.group(1) ;
                }
                // Humidity
                m = Pattern.compile("^<b>Relative Humidity:</b> (.*)").matcher(inputLine) ;
                if (m.matches()) {
                    humidity = m.group(1) ;
                }

                response += inputLine + "\n";
            }
            double windchill = 666.0;
            if (!"".equals(temp_f)) {
                windchill = Double.parseDouble(temp_f) ;
                if (!"".equals(wind_mph))
                    windchill = windchill(Double.parseDouble(temp_f),Double.parseDouble(wind_mph)) ;
                // System.out.println("windchill: " + windchill);
            }
            String sunrise_string = "" ;
            String sunset_string = "" ;
            //  Note: crappily named class Time is bundled in with the suntimes lib
            Time sunrise_UTC = new Time(0) ;
            Time sunset_UTC = new Time(0) ;
            try {
                sunrise_UTC = SunTimes.getSunriseTimeUTC(report_year, report_month, report_day, longitude, latitude, SunTimes.ZENITH) ;
                sunset_UTC = SunTimes.getSunsetTimeUTC(report_year, report_month, report_day, longitude, latitude, SunTimes.ZENITH) ;
            } catch (SunTimesException e) {
                e.printStackTrace() ;
            }
            TimeZone tz = null ;
            if (users.hasUser(username) && ! users.getUser(username).getTimeZoneString().equals("")) {
                tz = TimeZone.getTimeZone(users.getUser(username).getTimeZoneString()) ;
            }
            sunrise_string = sunString(sunrise_UTC, longitude, tz) ;
            sunset_string = sunString(sunset_UTC, longitude, tz) ;
            String sun_report = "<b>Sunrise:</b> " + sunrise_string + "\n<b>Sunset:</b> " + sunset_string;

            double score = getScore(wind_mph, wind_gust,temp_c,sky_conditions,weather_type,humidity,sunrise_UTC,sunset_UTC);
            double scoreRounded = Math.round(score*100)/100d;
            String short_response = temp_f + "F/" + temp_c + "C";
            boolean daylight = false;
            double fractionalHour = report_hour + report_minute / 60.0;
            if (sunrise_UTC.getFractionalHours() < sunset_UTC.getFractionalHours())
                daylight = (sunrise_UTC.getFractionalHours() < fractionalHour) && (sunset_UTC.getFractionalHours() > fractionalHour);
            else
                daylight = (sunrise_UTC.getFractionalHours() < fractionalHour) || (sunset_UTC.getFractionalHours() > fractionalHour);

            if (! sky_conditions.equals("")) {
                if(daylight)
                    short_response += ", " + sky_conditions.replace("clear", "sunny") ;
                else
                    short_response += ", " + sky_conditions.replace("clear", "moony") ;
            }
            if (! weather_type.equals("")) {
                short_response += ", " + weather_type ;
            }
            if (! wind_direction.equals("")) {
                short_response += ".  Wind " + wind_direction + " " + wind_mph + "mph" ;
                if (! wind_gust.equals("")) {
                    short_response += " gusting to " + wind_gust + "mph";
                }
            } else {
                short_response += ".  No temp_fwind" ;
            }
            short_response += ".  Humidity " + humidity ;
            //if (! precipitation.equals("none")) {
            //	short_response += ".  Precip last hour: " + precipitation ;
            //}
            String windchillString = "" ;
            Double d_temp_f = Double.parseDouble(temp_f);
            if (windchill != 666.0 && windchill != d_temp_f) {
                windchillString = "<b>Windchill:</b> " + String.format("%2.1fC", fToC(windchill)) + "\n" ;
                response += windchillString ;
                //if ((d_temp_f - windchill) > 2.0)
                //    response += windchillString;
            }
            if (! (sunrise_string.equals("") || sunset_string.equals(""))) {
                short_response +=  ".  " + sun_report ;
                response += sun_report ;
            }
            Date now = new Date();
            short_response += ".  Moon " + PhaseOfMoon.phaseAsShortString(now.getTime()) ;
            response += "\n<b>Moon:</b> " + PhaseOfMoon.phaseAsString(now.getTime()) ;
            if (0 != minutes_since_report) {
                short_response += ".  Reported " + minutes_since_report + " minutes ago at " + station ;
                response += "\nReported " + minutes_since_report + " minutes ago.";
            }
            response += "\n<b>Score: </b>" + scoreRounded;
            short_response += ".  Score " + scoreRounded + ".";

            String record="";

            /*if(wStore.checkSavedScores(score,username,station,short_response)) {
                record +=  BOLD + " New high score!" + BOLD;
            }*/



            String crecord = " " + checkRecordAttr("temp", "temperature", temp_c, username, station, short_response, false);
            String frecord = " " + checkRecordAttr("tempf", "temperature", temp_f, username, station, short_response, false);
            if(station.startsWith("K")) { //assume american
                record += frecord;
            } else {
                record += crecord;
            }
            if(!wind_mph.equals(""))
                record += " " + checkRecordAttr("wind", "wind speed", wind_mph, username, station, short_response, true);
            if(!wind_gust.equals(""))
                record += " " + checkRecordAttr("gust", "wind gust", wind_gust, username, station, short_response, true);

            if(!record.equals(""))
                record = BOLD + record + END_BOLD;

            response += record; short_response += record;

            //if (command.matches("(?i)fullw(a|e){2}th(a|e)r")) {
                return response;
            //} else {
            //    return short_response ;
            //}
        } catch  (SocketTimeoutException e) {
            return "I got bored waiting for the weather report for " + station ;
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            if(connection!=null) connection.disconnect();
            try {
                if(in!=null) in.close();
            } catch (IOException ioe) {
                System.out.println("Cannot close input stream");
                ioe.printStackTrace();
            }
        }
        return null;
    }

    private String checkRecordAttr(String attr, String attrName, String value, String username, String station, String report, boolean maxOnly) {
        //TODO quick bugfix, make it use doubles
        /*int valInt = (int) Math.round(Double.parseDouble(value));
        switch(wStore.checkRecordAttribute(attr, valInt, username, station, report)) {
            case -1:
                if(!maxOnly)
                    return "Record minimum " + attrName + "!!!";
                else return "";
            case 1:
                return "Record maximum " + attrName + "!!!";
            case 0:
                return "";
        }*/

        return "";
    }

    private double getScore(String wind_mph, String wind_gust,String temp_c,String sky_conditions, String weather_type,String humidity,Time sunrise_UTC,Time sunset_UTC) {
        double wind_mph_d=0, temp_c_d=0, wind_gust_d=0,humidity_d=50,bonus=0,sunHours=0;
        try {
            if(!wind_mph.equals(""))
                wind_mph_d = Integer.parseInt(wind_mph);
            if(!wind_gust.equals(""))
                wind_gust_d = Integer.parseInt(wind_gust);
            temp_c_d = Double.parseDouble(temp_c);
            if(!humidity.equals(""))
                humidity_d = Integer.parseInt(humidity.substring(0, humidity.length()-1));
            if( sunset_UTC.getFractionalHours()>sunrise_UTC.getFractionalHours() )
                sunHours = sunset_UTC.getFractionalHours() - sunrise_UTC.getFractionalHours();
            else
                sunHours = sunset_UTC.getFractionalHours() + 24 - sunrise_UTC.getFractionalHours();
            // debug
            // System.out.println("sunHours:" + sunHours);
            if( sky_conditions.contains("overcast") )
                bonus+=5;

            if( sky_conditions.contains("partly cloudy") )
                bonus+=1.5;
            else if( sky_conditions.contains("mostly cloudy") )
                bonus+=3;
            else if( sky_conditions.contains("cloudy") )
                bonus+=5;

            if( weather_type.contains("light rain"))
                bonus+=5;
            else if( weather_type.contains("heavy rain"))
                bonus+=15;
            else if( weather_type.contains("rain"))
                bonus+=10;

            if( weather_type.contains("haze"))
                bonus+=5;

            if( weather_type.contains("drizzle"))
                bonus+=5;
            if( weather_type.contains("freezing drizzle"))
                bonus+=10;
            if( weather_type.contains("ice pellets"))
                bonus+=15;
            if( weather_type.contains("blowing dust"))
                bonus+=10;

            if( weather_type.contains("light fog"))
                bonus+=5;
            else if( weather_type.contains("heavy fog"))
                bonus+=15;
            else if( weather_type.contains("ice fog"))
                bonus+=20;
            else if( weather_type.contains("ground fog"))
                bonus+=15;
            else if( weather_type.contains("fog"))
                bonus+=10;

            if(weather_type.contains("freezing spray"))
                bonus+=20;
            else if(weather_type.contains("freezing"))
                bonus+=15;

            if(weather_type.contains("tornado"))
                bonus+=100;
            if(weather_type.contains("volcanic ash"))
                bonus+=100;
            if(weather_type.contains("water spouts"))
                bonus+=50;
            if(weather_type.contains("blowing sand"))
                bonus+=50;
            if(weather_type.contains("frost"))
                bonus+=15;

            if(weather_type.contains("lightning"))
                bonus+=30;

            if(weather_type.contains("thunder"))
                bonus+=30;

            if( weather_type.contains("light ice pellets"))
                bonus+=10;
            else if( weather_type.contains("heavy ice pellets"))
                bonus+=20;
            else if( weather_type.contains("ice pellets"))
                bonus+=15;

            if( weather_type.contains("light ice crystals"))
                bonus+=20;
            else if( weather_type.contains("heavy ice crystals"))
                bonus+=30;
            else if( weather_type.contains("ice crystals"))
                bonus+=25;

            if( weather_type.contains("light sleet"))
                bonus+=10;
            else if( weather_type.contains("heavy sleet"))
                bonus+=20;
            else if( weather_type.contains("sleet"))
                bonus+=15;

            if( weather_type.contains("light snow"))
                bonus+=15;
            else if( weather_type.contains("heavy snow"))
                bonus+=25;
            else if( weather_type.contains("snow"))
                bonus+=20;

            if( weather_type.contains("smoke"))
                bonus+=75;

            //towering cumulonimbus is definitely threatening
            if( weather_type.contains("towering"))
                bonus += 7.5;

        } catch(NumberFormatException nfe) {
            System.out.println("oh no!");
            return 0;
        }
        if( humidity_d<50 )
            humidity_d += 2*(50-humidity_d);
        humidity_d = humidity_d/100;
        humidity_d -= 0.5;
        return (wind_mph_d/2) + Math.abs(15-temp_c_d) + ((wind_gust_d-wind_mph_d)/3) + humidity_d*Math.abs(temp_c_d)/2 + Math.abs(12-sunHours) + bonus;
    }

    private String sunString(Time t, double longitude, TimeZone tz) {
        String ret = "" ;
        GregorianCalendar cal = new GregorianCalendar(TimeZone.getTimeZone("UTC")) ;
        cal.set(Calendar.HOUR_OF_DAY, t.getHour()) ;
        cal.set(Calendar.MINUTE, t.getMinute()) ;
        if (null == tz) {
            // Fake time zone conversion
            // cal.add(Calendar.HOUR_OF_DAY, (int) longitude / 15) ;
        } else {
            // Real time zone conversion
            long tempdate = cal.getTimeInMillis() ;
            cal = new GregorianCalendar(tz) ;
            cal.setTimeInMillis(tempdate) ;
        }
        ret = ret + cal.get(Calendar.HOUR) + ":" ;
        if (10 > cal.get(Calendar.MINUTE)) {
            ret = ret + "0" + cal.get(Calendar.MINUTE) ;
        } else {
            ret = ret + cal.get(Calendar.MINUTE) ;
        }
        if ( cal.get(Calendar.AM_PM) == Calendar.AM ) {
            ret += "am" ;
        } else {
            ret += "pm" ;
        }
        if (null == tz) {
            ret += " GMT" ;
        }
        return ret ;
    }

    public double windchill(double t, double v) {
        double ret = t;
        if (t <= 50.0 && v > 3.0) {
            ret = 35.74 + 0.6215*t - 35.75*(Math.pow(v, 0.16)) + 0.4275*t*(Math.pow(v,0.16)) ;
        }
        return ret;
    }

    public double fToC (double f) {
        return (f - 32)*5/9;
    }

    public String[] getCommands() {
        return new String[]{"weather",
                "weathar", "waether", "fullweather", "fullweathar", "fullwaether", "raweather", "weatheraw", "metar", "rawmetar"};
    }



    public static void main(String[] args) {
    }

}

