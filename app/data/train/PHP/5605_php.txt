<?php require("../../includes/header.php"); ?><h1>Mike and Matrices</h1><div class="content">
<h3> Read problems statements in <a target="_blank" href="http://www.codechef.com/download/translated/LTIME07/mandarin/MIKE1.pdf">Mandarin Chinese</a> and <a target="_blank" href="http://www.codechef.com/download/translated/LTIME07/russian/MIKE1.pdf">Russian</a>.</h3>
<p>Mike is given a matrix <b>A</b>, <b>N</b> and <b>M</b> are numbers of rows and columns respectively. <b>A<sub>1, 1</sub></b> is the number in the top left corner. All the numbers in <b>A</b> are non-negative integers. He also has <b>L</b> pairs of integers (<b>i<sub>k</sub></b>, <b>j<sub>k</sub></b>). His task is to calculate <b>A<sub>i<sub>1</sub>, j<sub>1</sub></sub></b> + <b>A<sub>i<sub>2</sub>, j<sub>2</sub></sub></b> + ... + <b>A<sub>i<sub>L</sub>, j<sub>L</sub></sub></b>.
</p>
<p>
Unfortunately, Mike forgot if <b>A<sub>i, j</sub></b> was a number in the <b>i</b>'th row and <b>j</b>'th column or vice versa, if <b>A<sub>i, j</sub></b> was a number in the <b>j</b>'th row and <b>i</b>'th column.
</p>
<p>
So, Mike decided to calculate both <b>E<sub>1</sub></b> = <b>A<sub>i<sub>1</sub>, j<sub>1</sub></sub></b> + <b>A<sub>i<sub>2</sub>, j<sub>2</sub></sub></b> + ... + <b>A<sub>i<sub>L</sub>, j<sub>L</sub></sub></b> and <b>E<sub>2</sub></b> = <b>A<sub>j<sub>1</sub>, i<sub>1</sub></sub></b> + <b>A<sub>j<sub>2</sub>, i<sub>2</sub></sub></b> + ... + <b>A<sub>j<sub>L</sub>, i<sub>L</sub></sub></b>. If it is impossible to calculate <b>E<sub>1</sub></b>(i.e. one of the summands doesn't exist), then let's assume, that it is equal to <b>-1</b>. If it is impossible to calculate <b>E<sub>2</sub></b>, then let's also assume, that it is equal to <b>-1</b>.
</p>
<p>
Your task is to calculate <b>max(<b>E<sub>1</sub></b>, <b>E<sub>2</sub></b>)</b>.
</p>

<h3>Input</h3>
<p>
The first line contains two integers <b>N</b> and <b>M</b>, denoting the number of rows and the number of columns respectively.<br /><br />
Each of next <b>N</b> lines contains <b>M</b> integers. The <b>j</b>'th integer in the <b>(i + 1)</b>'th line of the input denotes <b>A<sub>i, j</sub></b>.<br />
</p>
<p>
The <b>(N + 2)</b>'th line contains an integer <b>L</b>, denoting the number of pairs of integers, that Mike has.<br /><br />
Each of next <b>L</b> lines contains a pair of integers. The <b>(N + 2 + k)</b>-th line in the input contains a pair (<b>i<sub>k</sub></b>, <b>j<sub>k</sub></b>).
</p>

<h3>Output</h3>
<p>The first line should contain an integer, denoting <b>max(<b>E<sub>1</sub></b>, <b>E<sub>2</sub></b>)</b>.</p>
<h3>Examples</h3>
<pre><b>Input:</b>
3 2
1 2
4 5
7 0
2
1 2
2 2
<b>Output:</b>
9
</pre>
<pre><b>Input:</b>
1 3
1 2 3
2
1 3
3 1
<b>Output:</b>
-1
</pre>
<pre><b>Input:</b>
1 3
1 2 3
2
1 1
3 1
<b>Output:</b>
4
</pre>
<h3>Explanation</h3>
<p>
In the first test case <b>N</b> equals to 3, <b>M</b> equals to 2, <b>L</b> equals to 2. <b>E<sub>1</sub></b> = 2 + 5 = 7, <b>E<sub>2</sub></b> = 4 + 5 = 9. The answer is <b>max(<b>E<sub>1</sub></b>, <b>E<sub>2</sub></b>)</b> = <b>max(</b>7<b>,</b> 9<b>)</b> = 9;
</p>
<p>
In the second test case <b>N</b> equals to 1, <b>M</b> equals to 3, <b>L</b> equals to 2. It is impossible to calculate <b>E<sub>1</sub></b> and <b>E<sub>2</sub></b>, because <b>A<sub>3, 1</sub></b> doesn't exist. So the answer is <b>max(<b>E<sub>1</sub></b>, <b>E<sub>2</sub></b>)</b> = <b>max(</b>-1<b>,</b> -1<b>)</b> = -1;
</p>
<p>
In the third test case <b>N</b> equals to 1, <b>M</b> equals to 3, <b>L</b> equals to 2. It is impossible to calculate <b>E<sub>1</sub></b>, because <b>A<sub>3, 1</sub></b> doesn't exist. So <b>E<sub>1</sub></b> is equal to -1. <b>E<sub>2</sub></b> = 1 + 3 = 4. The answer is <b>max(<b>E<sub>1</sub></b>, <b>E<sub>2</sub></b>)</b> = <b>max(</b>-1<b>,</b>4<b>)</b> = 4.
</p>

<h3>Scoring</h3>
<p>
1 ≤ <b>i<sub>k</sub></b>, <b>j<sub>k</sub></b> ≤ 500 for each test case.
</p>
<p>
Subtask 1 (10 points): 1 ≤ <b>N</b>, <b>M</b>, <b>L</b> ≤ 5, 0 ≤ <b>A<sub>i, j</sub></b> ≤ 10;<br /><br />
Subtask 2 (12 points): 1 ≤ <b>N</b>, <b>M</b>, <b>L</b> ≤ 300, 0 ≤ <b>A<sub>i, j</sub></b> ≤ 10<sup>6</sup>, all the numbers in <b>A</b> are equal;<br /><br />
Subtask 3 (20 points): 1 ≤ <b>N</b>, <b>M</b>, <b>L</b> ≤ 300, 0 ≤ <b>A<sub>i, j</sub></b> ≤ 10<sup>9</sup>;<br /><br />
Subtask 4 (26 points): 1 ≤ <b>N</b>, <b>M</b>, <b>L</b> ≤ 500, 0 ≤ <b>A<sub>i, j</sub></b> ≤ 10<sup>9</sup>;<br /><br />
Subtask 5 (32 points): 1 ≤ <b>N</b>, <b>M</b> ≤ 500, 1 ≤ <b>L</b> ≤ 250 000, 0 ≤ <b>A<sub>i, j</sub></b> ≤ 10<sup>9</sup>.<br />
</p>    </div><table cellspacing="0" cellpadding="0" align="left">
        <tr>
            <td width="14%">Author:</td>
            <td><a href="/users/kostya_by">kostya_by</a></td>
		</tr>
				                
        		<tr>
            <td width="14%">Date Added:</td>
            <td>28-11-2013</td>
        </tr>
        <tr>
            <td width="14%">Time Limit:</td>
            <td>1 sec</td>
        </tr>
        <tr>
            <td width="14%">Source Limit:</td>
            <td>50000 Bytes</td>
        </tr>
        <tr>
            <td width="14%">Languages:</td>
            <td>ADA, ASM, BASH, BF, C, C99 strict, CAML, CLOJ, CLPS, CPP 4.3.2, CPP 4.8.1, CPP11, CS2, D, ERL, FORT, FS, GO, HASK, ICK, ICON, JAR, JAVA, JS, LISP clisp, LISP sbcl, LUA, NEM, NICE, NODEJS, PAS fpc, PAS gpc, PERL, PERL6, PHP, PIKE, PRLG, PYTH, PYTH 3.1.2, RUBY, SCALA, SCM guile, SCM qobi, ST, TCL, TEXT, WSPC</td>
        </tr>
    </table> <?php require("../../includes/footer.php"); ?>