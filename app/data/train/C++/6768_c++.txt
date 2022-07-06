#include <iostream>
#include <string>
#include <string.h>
#include <stdlib.h>

using namespace std;

int main()
{
	int N, total, i;
	char alpha['z' + 1];
	size_t len;
	string frase;

	cin >> N;
	getline(cin, frase);

	while(N--)
	{
		memset(alpha, 0, 'z' + 1);

		getline(cin, frase);

		len = frase.size();
		while(len--)
		{
			if(frase[len] < 'a' || 'z' < frase[len] )
			{
				continue;
			}

			alpha[frase[len]] = 1;
		}

		for(total = 0, i = 'a'; i <= 'z'; i++)
		{
			if(alpha[i])
			{
				total++;
			}
		}

		cout << "frase ";

		if(total == 26)
		{
			cout << "completa";
		}
		else if(total >= 13)
		{
			cout << "quase completa";
		}
		else
		{
			cout << "mal elaborada";
		}

		cout << endl;
	}

	return 0;
}