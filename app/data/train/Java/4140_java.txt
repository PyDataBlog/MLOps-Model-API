
public class TestSea6Task1 {
	public static void main(String[] args) {
		String text = "Sun is shining. Today is a good day for test. Sun is shining. The students are happy. The birds are blue.";

		int indexSent = -1;
		int lengthSen = 0;
		int counterSen = 0;
		int indexLast = 0;
		int maxLengthSen = 0;

		for (int i = 0; i < text.length(); i++) {
			if (text.charAt(i) >= 'A' && text.charAt(i) <= 'Z') {
				counterSen++;
				lengthSen = i - indexLast;
				indexLast = i;
			}
			if (i == text.length() - 1) {
				lengthSen = text.length() - 1 - indexLast;

			}
			if (maxLengthSen < lengthSen) {
				maxLengthSen = lengthSen;
				indexSent = indexLast - maxLengthSen;
			}

		}
		String sentence = text.substring(indexSent, indexSent + maxLengthSen);
		System.out.println(sentence);
		System.out.println(counterSen);
	}

}
