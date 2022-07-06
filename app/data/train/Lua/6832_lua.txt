local palindrome = {}

function palindrome.is_palindrome(word)
    if string.len(word) <= 1 then
        return true
    end

    if palindrome.get_first_char(word) ~= palindrome.get_last_char(word) then
        return false
    end

    return palindrome.is_palindrome(palindrome.middle_chars(word))
end

function palindrome.get_first_char(word)
    return string.sub(word, 1, 1)
end

function palindrome.get_last_char(word)
    return string.sub(word, -1, -1)
end

function palindrome.middle_chars(word)
    return string.sub(word, 2, -2)
end

return palindrome
