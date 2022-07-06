/*
 * Search first occurence of a particular string in a given text [Finite Automata]
 * Author: Progyan Bhattacharya <progyanb@acm.org>
 * Repo: Design-And-Analysis-of-Algorithm [MIT LICENSE]
 */

#include "Search.h"

static int NextState(int m, char* pattern, int state, int symbol) {
    if (state < m && pattern[state] == symbol) {
        return state + 1;
    }
    for (int next = state, prev = next - 1, i = 0; next > 0; next--) {
        if (pattern[prev] == symbol) {
            for (i = 0; i < prev; i++) {
                if (pattern[i] != pattern[state - next + 1 + i]) {
                    break;
                }
            }
            if (i == prev) {
                return next;
            }
        }
    }
    return 0;
}

static void GenerateTable(int m, char* pattern, int Table[m][CHAR_MAX]) {
    for (int state = 0, symbol = 0; symbol < CHAR_MAX || (symbol = 0, ++state) < m; symbol++) {
        Table[state][symbol] = NextState(m, pattern, state, symbol);
    }
}

int Search(int n, char* haystack, int m, char* needle) {
    int Table[m + 1][CHAR_MAX], state = 0;
    GenerateTable(m + 1, needle, Table);
    for (int i = 0; i < n; i++) {
        state = Table[state][haystack[i]];
        if (state == m) {
            return (i - m + 1);
        }
    }
    return -1;
}
