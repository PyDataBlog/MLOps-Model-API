package main.java.srm149;

import java.util.HashSet;
import java.util.Set;

/*
 * SRM 149 DIV I Level 2
 * http://community.topcoder.com/stat?c=problem_statement&pm=1331
 */
public class MessageMess {
	private Set<String> _dictionarySet = new HashSet<String>();
	
	public String restore(String[] dictionary, String message) {
		populateDictionarySet(dictionary);
		int numAnswers = 0;
		StringBuilder answer = findPossibleAnswer(0, message, "");
		int ansLength = answer.length();
		//Find answer
		if (ansLength  > 0 && ansLength  < message.length()) {
			answer = answer.deleteCharAt(ansLength - 1);
			ansLength = answer.length();
			answer = findPossibleAnswer(ansLength, message, answer.toString());
		} else if ( ansLength > 0) {
			//Find multiple answers
			numAnswers++;
			StringBuilder alternative = new StringBuilder(answer.substring(0, answer.indexOf(" ")));
			ansLength = alternative.length();
			alternative = findPossibleAnswer(ansLength, message, alternative.toString());
			if (alternative.length() >= message.length()) {
				numAnswers++;
			}
		}
		
		if(numAnswers > 1) {
			answer = new StringBuilder("AMBIGUOUS!");
		} else if(answer.length() == 0 || answer.length() < message.length()) {
			answer = new StringBuilder("IMPOSSIBLE!");
		} else {
			answer.deleteCharAt(answer.length() - 1);
		}
		return answer.toString();
		
	}
	
	private StringBuilder findPossibleAnswer(int startIndex, String message, String prefix) {
		StringBuilder possibleWord = new StringBuilder(prefix);
		StringBuilder answer = new StringBuilder();
		for(int i = startIndex; i < message.length(); i++) {
			possibleWord.append(message.charAt(i));
			if(_dictionarySet.contains(possibleWord.toString())) {
				answer.append(possibleWord).append(" ");
				possibleWord.delete(0, possibleWord.length());
			}
		}
		return answer;
	}
	
	private void populateDictionarySet(String[] array) {
		for(int i = 0; i < array.length; i++) {
			_dictionarySet.add(array[i]);
		}
	}

}
