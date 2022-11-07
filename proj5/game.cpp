//
//  game.cpp
//  Project5
//
//  Created by Joyce Chen on 11/3/22.
//

#include <iostream>
#include <cctype>
#include <cstring>
#include "utilities.h"
#include <limits>
using namespace std;

int playOneRound(const char words[][7], int nWords, int wordnum);
bool wordExists(const char words[][7], int nWords, char* word);
bool isValidWord(const char* word);
const int MAXWORDS = 9000;
const int MAXWORDLENGTH = 6;

int main()
{
    cout.setf(ios::fixed);
    cout.precision(2);
    char words[MAXWORDS][MAXWORDLENGTH + 1];
    
    int numWords = getWords(words, MAXWORDS, "/Users/joycechen/cs31/words.txt");
    if(numWords < 1 || numWords > MAXWORDS){
        cout << "No words were loaded, so I can't play the game." << endl;
        return 0;
    }
    int rounds;
    int total = 0;
    double average;
    int minScore = INT_MAX;
    int maxScore = INT_MIN;
    
    cout << "How many rounds do you want to play? ";
    cin >> rounds;
    
    if(rounds < 0){
        cout << "The number of rounds must be positive." << endl;
        return 0;
    }
    
    cout << endl;
    for(int i = 1; i <= rounds; i++){
        cout << "Round " << i << endl;
        int randomInt = randInt(0, numWords - 1);
//        cout << words[randomInt] << endl;
        int score = playOneRound(words, numWords, randomInt);
        if(score < minScore){
            minScore = score;
        }
        if (score > maxScore){
            maxScore = score;
        }
        if(score == 1){
            cout << "You got it in 1 try." << endl;
        }else{
            cout << "You got it in " << score << " tries." << endl;
        }
        total += score;
        average = total/i;
        cout << "Average: " << average << ", minimum: " << minScore << ", maximum: " << maxScore << endl << endl;
    }
    return 0;
}

int playOneRound(const char words[][7], int nWords, int wordnum){
    if(nWords < 0 || wordnum < 0 || wordnum >= nWords){
        return -1;
    }
    //length of the answer
    const int length = static_cast<int>(strlen(words[wordnum]));

    int golds = 0;
    int silvers = 0;
    int count = 0;

    while(golds != length){
        char word[MAXWORDLENGTH + 1];
        strcpy(word, words[wordnum]);
        golds = 0;
        silvers = 0;
        
        char userWord[MAXWORDLENGTH + 1];
        cout << "Probe word: ";
        cin >> userWord;
        
        int userLen = strlen(userWord);
        if(userLen < 4 || userLen > 6 || !isValidWord(userWord)){
            cout << "Your probe word must be a word of 4 to 6 lower case letters." << endl;
        }
        else if (!wordExists(words, nWords, userWord)){
            cout << "I don't know that word." << endl;
        }
        else{
            for(int i = 0; i < strlen(userWord); i++){
                if(i < strlen(word) && userWord[i] == word[i]){
                    golds++;
                    word[i] = '1';
                } else{
                    for(int j = 0; j < strlen(word); j++){
                        if(word[j] != '1' && userWord[i] == word[j]){
                            silvers++;
                            word[j] = '1';
                        }
                    }
                }
            }
            if(golds != length){
                cout << "Golds: " << golds << ", Silvers: " << silvers << endl;
            }
            count++;
        }
    }
    return count;
}

bool wordExists(const char words[][7], int nWords, char* word){
    for(int i = 0; i < nWords; i++){
        if(strcmp(words[i],word) == 0){
            return true;
        }
    }
    return false;
}

bool isValidWord(const char* word){
    for(int i = 0; i < strlen(word); i++){
        if(isupper(word[i]) || !isalpha(word[i])){
            return false;
        }
    }
    return true;
}
