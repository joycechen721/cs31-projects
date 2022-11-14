//
//  game.cpp
//  Project5 - really similar to MasterMind & Wordle
//
//  Created by Joyce Chen on 11/3/22.
//

#include <iostream>
#include <cctype>
#include <cstring>
#include "utilities.h"
using namespace std;

int playOneRound(const char words[][7], int nWords, int wordnum);
bool wordExists(const char words[][7], int nWords, char* word);
bool isValidWord(const char* word);
const int MAXWORDS = 9000;
const int MAXWORDLENGTH = 6;
const char WORDFILENAME[] = "/Users/joycechen/cs31/words.txt";

int main()
{
    cout.setf(ios::fixed);
    cout.precision(2);
    char words[MAXWORDS][MAXWORDLENGTH + 1];
    
    //fill words array with words from file
    int numWords = getWords(words, MAXWORDS, WORDFILENAME);
    
    //check if getWords returns a negative # or # greater than max limit
    if(numWords < 1 || numWords > MAXWORDS){
        cout << "No words were loaded, so I can't play the game." << endl;
        return 0;
    }
    
    int rounds;
    double total = 0;
    double average;
    int minScore = 0;
    int maxScore = 0;
    
    //prompt user for #of rounds
    cout << "How many rounds do you want to play? ";
    cin >> rounds;
    cin.ignore(10000, '\n');
    
    //check for invalid rounds input
    if(rounds <= 0){
        cout << "The number of rounds must be positive." << endl;
        return 0;
    }
    cout << endl;
    
    //playing each round
    for(int i = 1; i <= rounds; i++){
        cout << "Round " << i << endl;
        //get a random integer in the range of numWords to serve as this round's answer
        int randomInt = randInt(0, numWords - 1);
        cout << "The hidden word is " << strlen(words[randomInt]) << " letters long." << endl;
        
        //play the round and get its score
        int score = playOneRound(words, numWords, randomInt);
        if(i == 1 || score < minScore){
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
        //add this round's score to overall total
        total += score;
        //calculate the average
        average = total/i;
        cout << "Average: " << average << ", minimum: " << minScore << ", maximum: " << maxScore << endl << endl;
    }
    return 0;
}

//responsible for 1 game round's functionality
int playOneRound(const char words[][MAXWORDLENGTH + 1], int nWords, int wordnum){
    if(nWords <= 0 || wordnum < 0 || wordnum >= nWords){
        return -1;
    }
    //length of the answer string
    const int length = static_cast<int>(strlen(words[wordnum]));

    int golds = 0;   //stores each guess's # of golds
    int silvers = 0; //stores each guess's # of silvers
    int count = 0;   //stores each round's # of tries

    while(golds != length){
        //store answer string into a C string variable
        char word[MAXWORDLENGTH + 1];
        strcpy(word, words[wordnum]);
        
        //reset golds & silvers to 0 after each guess
        golds = 0;
        silvers = 0;
        
        //prompt user for a guess
        char userWord[101];
        cout << "Probe word: ";
        cin.getline(userWord, 100);
    
        //check if user guess is valid
        if(strlen(userWord) < 4 || strlen(userWord) > 6 || !isValidWord(userWord)){
            cout << "Your probe word must be a word of 4 to 6 lower case letters." << endl;
        }
        else if (!wordExists(words, nWords, userWord)){
            cout << "I don't know that word." << endl;
        }
        //user guess is valid at this point
        else{
            //loop through the user's guess, counting golds
            for(int i = 0; userWord[i] != '\0'; i++){
                //if character at user's guess is same as that in answer string (same position)
                if(i < strlen(word) && userWord[i] == word[i]){
                    golds++;
                    //mark seen characters
                    userWord[i] = '1';
                    word[i] = '1';
                }
            }
            //loop through user's guess, counting silvers
            for(int i = 0; userWord[i] != '\0'; i++){
                //loop through answer string
                for(int j = 0; word[j] != '\0'; j++){
                    //check if the character in user's guess matches any other character in answer string
                    if(userWord[i] != '1' && word[j] != '1' && userWord[i] == word[j]){
                        silvers++;
                        //mark seen characters
                        userWord[i] = '1';
                        word[j] = '1';
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

//returns true only if word is inside an array
bool wordExists(const char words[][7], int nWords, char* word){
    for(int i = 0; i < nWords; i++){
        if(strcmp(words[i],word) == 0){
            return true;
        }
    }
    return false;
}

//returns true only if word doesn't have uppercase or non-alphabetical letters
bool isValidWord(const char* word){
    for(int i = 0; word[i] != '\0'; i++){
        if(isupper(word[i]) || !isalpha(word[i]) || isspace(word[i])){
            return false;
        }
    }
    return true;
}
