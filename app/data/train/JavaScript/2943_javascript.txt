// Write a program that extracts from a given text all palindromes, e.g. "ABBA", "lamal", "exe".

function findPalindromes(input) {

    var words = input.replace(/\W+/g, ' ').replace(/\s+/, ' ').trim().split(' '),
        palindromes = [],
        length = words.length,
        currentWord,
        i;

    for (i = 0; i < length; i += 1) {
        currentWord = words[i];
        if (isPalindrome(currentWord) && currentWord.length != 1) {
            palindromes.push(currentWord);
        }
    }

    return palindromes.join();
}

function isPalindrome(currentWord) {
    var length = currentWord.length,
        i;

    for (i = 0; i < length / 2; i += 1) {
        if (currentWord[i] != currentWord[currentWord.length - 1 - i]) {
            return false;
        }
    }

    return true;
}

var textSample = 'Write a program that extracts from a given text all palindromes, e.g. "ABBA", "lamal", "exe".';

console.log(findPalindromes(textSample));
