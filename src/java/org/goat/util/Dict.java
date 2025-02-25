package org.goat.util;

import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;

/**
 * A general utility class for accessing a dictionary.
 *
 * @author encontrado, bc
 *
 */
public class Dict {

    private RandomAccessFile rafDict;
    private static final File DICTFILE = new File("resources/words");
    private RandomAccessFile rafIndex;
    private static final File INDEXFILE = new File("resources/words.index");
    public int numWords;


    /**
     * This no-args constructor exits if for whatever reason the Dict resource does not exist.
     * <p/>
     * And to build the index file if it doesn't exist
     * <p/>
     * And to rebuild the index file if the length recorded in its header doesn't match the length of the Dict resource.
     */
    public Dict() {
        try {
            rafDict = new RandomAccessFile(DICTFILE, "r");
        } catch (FileNotFoundException fnfe) {
            System.out.println("dict file does not exist.");
            fnfe.printStackTrace();
            System.exit(1);
        }
        try {
            rafIndex = new RandomAccessFile(INDEXFILE, "rw");
        } catch (FileNotFoundException fnfe) {
            System.out.println("Could not open file \'" + DICTFILE.toString() + "\" for writing.");
            fnfe.printStackTrace();
            System.exit(1);
        }
        try {
            if (0 == rafDict.length()) {
                System.out.println("I refuse to work with zero-length dictionary files!");
                System.exit(1);
            }
            if (0 == rafIndex.length()) {
                System.out.println("I refuse to work with zero-length index files.  Rebuilding index...");
                if (!buildIndex()) {
                    System.out.println("Oh, screw it.  I can't index that.  I quit.");
                    System.exit(1);
                }
            }
            // we don't need this variable, but I can't resist.
            long dictLength = rafDict.length();
            rafIndex.seek(0);
            // rebuild index if length of header doesn't match length of... Dict.
            if (dictLength != (long) rafIndex.readInt()) {
                System.out.println("Dict length mismatch!  Rebuilding index...");
                // do you still wonder why there aren't more women in this field?
                if (!buildIndex()) {
                    System.out.println("Alas!  I could not build ze index!  I expire!");
                    System.exit(1);
                }
            }
            // kill this next test once we're all confident and debugged
            rafIndex.seek(0);
            if (dictLength != (long) rafIndex.readInt()) {
                System.out.println("Something has gone horribly wrong...");
                System.out.println("dictLength: " + dictLength);
                rafIndex.seek(0);
                System.out.println("rafIndex.readInt() : " + rafIndex.readInt());
                System.exit(1);
            }
            numWords = (int) rafIndex.length() / 4 - 1;
        } catch (IOException e) {
            e.printStackTrace();
            //System.exit(1);  // this is a Bad Thing.
        }
    }

    /**
     * Just gets a totally random word from the dictionary file.
     * <p/>
     * Now with hyper-index functioning!
     *
     * @return A random word
     */
    public String getRandomWord() {
        return getWord((int) (Math.random() * numWords) + 1);
    }

    /**
     * Gets all the words in the dictionary which have letters that match the supplied string. Ignores spaces around the
     * string, and case.
     *
     * @param targetWord The word you are seeking all matches with.
     * @return An ArrayList of the matching words
     */
    public ArrayList<String> getMatchingWords(String targetWord) {
        targetWord = targetWord.trim();
        char[] targetWordArray = targetWord.toLowerCase().toCharArray();
        Arrays.sort(targetWordArray);	//presort this, save some time later
        String word;
        ArrayList<String> validWords = new ArrayList<String>(750);
        try {
            BufferedReader br = new BufferedReader(new FileReader(DICTFILE));
            while ((word = br.readLine()) != null) {
                word = word.toLowerCase();
                if (checkWord(word.toCharArray(), targetWordArray))
                    validWords.add(word);
            }
            br.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
        validWords.trimToSize();
        return validWords;
    }

    public boolean contains(String word) {
        word = word.toLowerCase();
        word = word.trim();
        //OK let's use a bsearch
        int guessPos = numWords / 2;
        int ceiling = numWords + 1, floor = 1, oldCeiling = 1, oldFloor = 2;
        do {
            String guessWord = getWord(guessPos);
            guessWord = guessWord.toLowerCase();
            if (guessWord == null)
                return false;
            int ComparisonResult = compare(word, guessWord);
            if (ComparisonResult < 0)
                ceiling = guessPos;
            else if (ComparisonResult > 0)
                floor = guessPos;
            else
                return true;
            guessPos = (ceiling - floor) / 2 + floor;
            if (ceiling == floor || oldCeiling == ceiling && oldFloor == floor)
                return false;
            oldCeiling = ceiling;
            oldFloor = floor;
        } while (true);
        //	return false;
    }

    private int compare(String word1, String word2) {
        if (word1.equals(word2))
            return 0;
        String[] ordered = {word1, word2};
        Arrays.sort(ordered);
        if (ordered[0].equals(word1))
            return -1;
        else if (ordered[0].equals(word2))
            return 1;
        return 0;
    }

    /*protected void finalize() throws Throwable {
        try {
            rafDict.close();
            rafIndex.close();
        } finally {
            super.finalize();
        }
    }*/

	/*
	  You probably want to use numWords instead of this; it's faster.
	 *
	 * @param file File to be line-counted
	 * @return number of lines in File

	private int lineCount(File file) {
		int count = 0;
		try {
			BufferedReader in = new BufferedReader(new FileReader(file));
			String line = in.readLine();
			while (line != null) {
				count++;
				line = in.readLine();
			}
			in.close();
		} catch (FileNotFoundException e) {
			System.out.println("The dictionary seems to have vanished!");
			e.printStackTrace();
			System.exit(1);
		} catch (IOException e) {
			e.printStackTrace();
			System.exit(1);
		}
		return count;
	} */

    /**
     * Checks if a given word is valid for current char arraylist.
     * Note: targetWord has to be pre-sorted (alphabetically) before it gets here
     *
     * @param word Word to be checked.
     * @param targetWord A sorted word, in alphabetical order.
     * @return True if matches, false if not.
     */
    private boolean checkWord(char[] word, char[] targetWord) {
        Arrays.sort(word);
        int floor = 0;
        int hits = 0;
        int tlength = targetWord.length;
        int wlength = word.length;
        for (int i = 0; i < wlength; i++)
            for (int j = floor; j < tlength; j++) {
                if (word[i] < targetWord[j])
                    return false;
                if (word[i] > targetWord[j])
                    continue;
                floor = j + 1;
                hits++;
                break;
            }
        if (hits == wlength)
            return true;
        return false;
    }

    /**
     * (re)Builds the word index.
     * <p/>
     * Note raw word file should not exceed INT.MAX_VALUE bytes. We use int instead of long throughout this class mainly to
     * keep the size of the index file down.
     *
     * @return True if index is successfully built
     */
    private boolean buildIndex() {

        try {
            rafDict.seek(0);
            rafIndex.seek(0);
            //write the 'header' of the index
            //length of file
            rafIndex.writeInt((int) rafDict.length());
            int count = 0;
            while ((int) rafDict.getFilePointer() < Integer.MAX_VALUE && rafDict.getFilePointer() < rafDict.length()) {
                rafIndex.writeInt((int) rafDict.getFilePointer());
                rafDict.readLine();
                ++count;
            }
            rafIndex.setLength(rafIndex.getFilePointer());
            System.out.println("indexed " + count + " words.");
            return true;
        } catch (IOException e) {
            e.printStackTrace();
            System.exit(1);
        }
        return false;
    }

    /**
     * get nth word in word file, using index file
     *
     * @param num word number
     * @return String
     */
    public String getWord(int num) {
        if (num > numWords) {
            //complain
            System.out.println("I don't have " + num + " words!");
            // the java-like thing to do here is probably to throw
            // an exception, but that just seems excessive.  Also,
            // I don't know how to monkey with exceptions yet.
            return null;
        } else if (num < 1) {
            System.out.println("word number must be 1 or greater, and no more than " + numWords);
            return null;
        }
        try {
            rafIndex.seek((long) (num * 4));
            rafDict.seek((long) rafIndex.readInt());
            return rafDict.readLine();
        } catch (IOException e) {
            e.printStackTrace();
            System.exit(1);
        }
        return null;
    }


    //  Main method here for debugging

    public static void main(String[] args) {
        Dict dict = new Dict();
        System.out.println("Some random words, hopefully:\n\n");
        for (int i = 0; i < 100; i++)
            System.out.print(dict.getRandomWord() + ' ');
		/* now check bsearch
		//This next takes a while
		System.out.println("\n\nTesting contains() on all words in index, be patient:\n\n");
		for (int i = 1; i <= dict.numWords; i++) {
			String word = dict.getWord(i) ;
			if ( ! dict.contains(word) ) {
				System.out.println(word + " :contains() returns: " + dict.contains(word));
			}
		}*/
        System.out.println("\n\nAnd these false:\n\n");
        System.out.println("poopmastah" + " :contains() returns: " + dict.contains("poopmastah"));
        System.out.println("assedsr" + " :contains() returns: " + dict.contains("assedsr"));
        System.out.println("zogg" + " :contains() returns: " + dict.contains("zogg"));
        System.out.println("aaaaa" + " :contains() returns: " + dict.contains("aaaaa"));
        System.out.println("zzzzz" + " :contains() returns: " + dict.contains("zzzzz"));
        // don't need to test this, it's built into the constructor, if needed
        //System.out.println("\n\nBuilding index...\n\n");
        //dict.buildIndex() ;
        System.out.println("\n\nSmall index file check:\n\n");
        System.out.println("first word: " + dict.getWord(1));
        System.out.println("Word #666: " + dict.getWord(666));
        System.out.println("indexed words: " + dict.numWords);
        System.out.println("last word : " + dict.getWord(dict.numWords));
        System.out.println("trying to produce an error by requesting word #" + (dict.numWords + 1) + " : ");
        System.out.println(dict.getWord(dict.numWords + 1));
        System.out.println("this should get another error, requesting word #0 (i.e., before #1) :");
        System.out.println(dict.getWord(0));
        System.out.println("\n\nSmall index file check:\n\n");
        System.out.println("Finding matches for \"rubellas\"") ;
        ArrayList<String> matches = dict.getMatchingWords("rubellas") ;
        System.out.println(matches.size() + " matches found:") ;
        System.out.println(matches) ;

        System.out.println("\nDoing a quick benchmark of checkWord2, and getMatchingWords.");
        long time1 = System.currentTimeMillis();
        for(int i=0;i<20;i++) {
            dict.getMatchingWords("electroencephalographies");
            dict.getMatchingWords("absolutes");
            dict.getMatchingWords("door");
        }
        System.out.println("Benched: " + (System.currentTimeMillis() - time1)/1000.0/60.0 + " seconds per getMatchingWords()");
        time1 = System.currentTimeMillis();
        for(int i=0;i<2000;i++) {
            dict.checkWord("electroencephalographies".toCharArray(), "graphs".toCharArray());
            dict.checkWord("absolutes".toCharArray(), "sots".toCharArray());
            dict.checkWord("monsters".toCharArray(), "young".toCharArray());
        }
        System.out.println("Benched: " + (System.currentTimeMillis() - time1)/1000.0/6000.0 + " seconds per checkWord2()");
        System.out.println("OldCheckwords: " + dict.getMatchingWords("electroencephalographies").size());
        System.out.println("OldCheckwords: " + dict.getMatchingWords("absolutes").size());

        //This next takes long, long while
        System.out.println("\n\nBig index file check:\n\n");
        System.out.println("\n\nTesting getMatchingWords() on all words in index, be patient:\n\n");
        int maxMatches = 0 ;
        String maxMatchWords = "" ;
        int progressIndicator = 1 ;
        ArrayList<String> wordMatches = new ArrayList<String>() ;
        for (int i = 1; i <= dict.numWords; i++) {
            String word = dict.getWord(i) ;
            wordMatches = dict.getMatchingWords(word) ;
            if (0 == matches.size())
                System.out.println(word + ": no matches found!") ;
            if (wordMatches.size() > maxMatches) {
                maxMatches = wordMatches.size() ;
                maxMatchWords =  maxMatchWords + " " + word + ":" + wordMatches.size();
            }
            if (progressIndicator >= 1000) {
                System.out.print(i + "...") ;
                progressIndicator = 1 ;
            } else {
                progressIndicator++ ;
            }
        }
        System.out.println("Max number of matches: " + maxMatches) ;
        System.out.println("Some words, with number of matches: " + maxMatchWords) ;

        System.out.println("\nDone.");
    }

}

