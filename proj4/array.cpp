//
//  main.cpp
//  Project4
//
//  Created by Joyce Chen on 10/27/22.
//

#include <iostream>
#include <string>
#include <cassert>
#include <cctype>
using namespace std;

int appendToAll(string a[], int n, string value);
int lookup(const string a[], int n, string target);
int positionOfMax(const string a[], int n);
int rotateLeft(string a[], int n, int pos);
int countRuns(const string a[], int n);
int flip(string a[], int n);
int differ(const string a1[], int n1, const string a2[], int n2);
int subsequence(const string a1[], int n1, const string a2[], int n2);
int lookupAny(const string a1[], int n1, const string a2[], int n2);
int split(string a[], int n, string splitter);

int main() {
    cout << "Testing started" << endl;

    //appendToAll
    string a[3] = { "rishi", "margaret", ""};
    assert(appendToAll(a, 3, "?") == 3 && a[0] == "rishi?" && a[1] == "margaret?" && a[2] == "?");
    //append to no elements in array
    assert(appendToAll(a, 0, "?") == 0 && a[0] == "rishi?" && a[1] == "margaret?");
    //negative n input
    assert(appendToAll(a, -3, "") == -1);
    //append empty string
    assert(appendToAll(a, 3, "") == 3 && a[0] == "rishi?" && a[2] == "?");
    //append to no elements
    assert(appendToAll(a, 0, "!!") == 0);

    //lookup
    string people[5] = { "boris", "gordon", "rishi", "liz", "john" };
    assert(lookup(people, 5, "gordon") == 1);
    //string not in array
    assert(lookup(people, 5, "") == -1);
    //case sensitivity
    assert(lookup(people, 5, "Gordon") == -1);
    //only look in partial array
    assert(lookup(people, 3, "liz") == -1);
    string people1[5] = {"boris", "rishi", "rishi", "boris", "rishi"};
    //multiple targets in array
    assert(lookup(people1, 5, "rishi") == 1);
    //target is first element
    assert(lookup(people1, 5, "boris") == 0);
    //negative n input
    assert(lookup(people1, -4, "rishi") == -1);
    //no elements in array
    assert(lookup(people, 0, "") == -1);

    //positionOfMax
    string h[7] = { "rishi", "margaret", "gordon", "tony", "", "john", "liz" };
    assert(positionOfMax(h,7) == 3);
    //part of the array
    assert(positionOfMax(h, 2) == 0);
    //array of empty strings
    string g[5] = {};
    assert(positionOfMax(g,5) == 0);
    //nonunique string elements
    string y[4] = {"hello", "hello","hello","hello"};
    assert(positionOfMax(y, 4) == 0);
    //no elements in array
    assert(positionOfMax(h, 0) == -1);
    //increasing order, max is last element
    string alphabet[5] = {"a", "b", "c", "d", "e"};
    assert(positionOfMax(alphabet, 5) == 4);
    //decreasing order, max is first element
    string alphabet2[5] = {"e", "d", "c", "b", "a"};
    assert(positionOfMax(alphabet2, 5) == 0);

    //rotateLeft
    assert(rotateLeft(people, 5, 2) == 2 && people[0] == "boris" && people[1] == "gordon" && people[2] == "liz" && people[3] == "john" && people[4] == "rishi");
    //position greater than array size
    assert(rotateLeft(people, 5, 8) == -1);
    //rotate from first index
    assert(rotateLeft(people, 5, 0) == 0 && people[0] == "gordon" && people[1] == "liz" && people[2] == "john" && people[3] == "rishi" && people[4] == "boris");
    //array of empty strings
    assert(rotateLeft(g, 5, 0) == 0 && g[0] == "" && g[3] == "" && g[4] == "");
    //negative array size
    assert(rotateLeft(h, -4, 4) == -1);
    //rotate from last index
    assert(rotateLeft(people, 5, 4) == 4 && people[0] == "gordon" && people[4] == "boris");

    //countRuns
    string d[9] = {"tony", "boris", "rishi", "rishi", "rishi", "gordon", "gordon", "gordon", "rishi"};
    assert(countRuns(d, 9) == 5);
    //all unique elements
    assert(countRuns(people, 5) == 5);
    //all same elements
    assert(countRuns(g, 5) == 1);
    //same first & last elements
    string c[6] = {"", "tony", "tony", "arav", "james", ""};
    assert(countRuns(c, 6) == 5);
    //call part of array
    assert(countRuns(c, 4) == 3);
    //negative array size
    assert(countRuns(c, -1) == -1);
    //no elements in array
    assert(countRuns(d, 0) == 0);

    //flip
    //array with odd # of elements
    assert(flip(people, 5) == 5 && people[0] == "boris" && people[2] == "john" && people[4] == "gordon");
    //array with even # of elements
    assert(flip(c, 6) == 6 && c[0] == "" && c[2] == "arav" && c[3] == "tony" && c[5] == "");
    //array of empty strings
    assert(flip(g, 5) == 5 && g[0] == "" && g[3] == "" && g[4] == "");
    //no elements in array
    assert(flip(c, 0) == 0);
    //array of size 1
    string one[1] = {"hi"};
    assert(flip(one, 1) == 1 && one[0] == "hi");
    //negative array size
    assert(flip(one, -1) == -1);

    //differ
    string leader[6] = { "boris", "rishi", "", "tony", "theresa", "david" };
    string politician[5] = { "boris", "rishi", "david", "", "tony" };
    string sodas[6] = {"fanta", "sprite", "pepsi", "cola", "seven-up", "dr. pepper"};
    assert(differ(leader, 6, politician, 5) == 2);
    //arrays are the same
    assert(differ(leader, 6, leader, 6) == 6);
    //a2 runs out
    assert(differ(leader, 2, politician, 1) == 1);
    //a1 runs out
    assert(differ(politician, 1, leader, 2) == 1);
    //arrays are entirely different
    assert(differ(people, 5, sodas, 6) == 0);
    //negative array sizes
    assert(differ(people, -5, sodas, 6) == -1);
    assert(differ(people, 5, sodas, -1) == -1);
    //inspecting no elements
    assert(differ(people, 5, sodas, 0) == 0);

    //subsequence
    string o[7] = {"banana", "orange", "apple", "kiwi", "grapes", "berries", "pineapple"};
    string p[3] = {"kiwi", "grapes", "berries"};
    assert(subsequence(o, 7, p, 3) == 3);
    string p1[3] = {"banana", "apple", "kiwi"};
    //same but non-consecutive elements
    assert(subsequence(o, 7, p1, 3) == -1);
    //empty sequences are subsequence to any array
    assert(subsequence(p1, 3, p, 0) == 0);
    assert(subsequence(p1, 0, p1, 0) == 0);
    //a2 size greater than a1
    assert(subsequence(p, 3, o, 7) == -1);
    string p2[3] = {"pineapple", "pineapple", "grapes"};
    //subsequence check at end of a1
    assert(subsequence(o, 7, p2, 3) == -1);
    //subsequence at last element
    string p3[1] = {"pineapple"};
    assert(subsequence(o, 7, p3, 1) == 6);
    //subsequence only at first element
    string p4[1] = {"banana"};
    assert(subsequence(o, 7, p4, 1) == 0);

    //lookupAny
    assert(lookupAny(o, 7, p, 3) == 3);
    //first occurrence at index 0
    string p9[3] = {"apple", "apple", "banana"};
    assert(lookupAny(o, 7, p9, 3) == 0);
    //no occurrences
    assert(lookupAny(leader, 6, p1, 3) == -1);
    //no elements in array
    assert(lookupAny(politician, 5, sodas, 0) == -1);
    //same arrays
    assert(lookupAny(people, 5, people, 5) == 0);

    //split
    assert(split(o, 7, "mango") == 5);
    for(int i = 0; i < 7; i++){
        cout << o[i] << " ";
    }
    cout << endl;
    
    //splitter inside array, in correct position
    assert(split(politician, 5, "david") == 2);
    for(int i = 0; i < 5; i++){
        cout << politician[i] << " ";
    }
    cout << endl;
    
    //splitter inside array, not in correct position
    string pm2[4] = { "margaret", "theresa", "liz", "rishi" };
    assert(split(pm2, 4, "rishi") == 2);
    for(int i = 0; i < 4; i++){
        cout << pm2[i] << " ";
    }
    cout << endl;
    
    //splitter as multiple array elements
    string sames[8] = {"rishi", "abe", "sarah", "kinsely", "rishi", "lily", "rishi", "rishi"};
    assert(split(sames, 8, "rishi") == 3);
    for(int i = 0; i < 8; i++){
        cout << sames[i] << " ";
    }
    cout << endl;
    
    //splitter equivalent to all elements
    assert(split(g, 5, "") == 0);
    for(int i = 0; i < 5; i++){
        cout << g[i] << " ";
    }
    cout << endl;
    
    //splitter at first element
    assert(split(alphabet, 5, "a") == 0);
    for(int i = 0; i < 5; i++){
        cout << alphabet[i] << " ";
    }
    cout << endl;
    
    //splitter at last element
    assert(split(alphabet, 5, "e") == 4);
    for(int i = 0; i < 5; i++){
        cout << alphabet[i] << " ";
    }
    cout << endl;
    
    //splitter smaller than all elements
    assert(split(alphabet, 5, "") == 0);
    for(int i = 0; i < 5; i++){
        cout << alphabet[i] << " ";
    }
    cout << endl;
    
    //splitter greater than all elements
    assert(split(alphabet, 5, "f") == 5);
    for(int i = 0; i < 5; i++){
        cout << alphabet[i] << " ";
    }
    cout << endl;
    
    //no elements in array
    assert(split(sames, 0, "") == 0);
    
    cout << "All tests succeeded" << endl;
    
    return 0;
}

int appendToAll(string a[], int n, string value){
    if(n < 0){
        return -1;
    }
    //concatenating value to each string element in a a
    for (int i = 0; i < n; i++){
        a[i] += value;
    }
    return n;
}

int lookup(const string a[], int n, string target){
    for(int i = 0; i < n; i++){
        if(a[i] == target){
            //returns smallest index if there's multiple targets
            return i;
        }
    }
    return -1;
}

int positionOfMax(const string a[], int n){
    //if array is empty or n is invalid
    if(n <= 0){
        return -1;
    }
    int maxIndex = 0;
    string maxString = a[0];
    for(int i = 1; i < n; i++){
        //update position only if element is greater than current max
        if(a[i] > maxString){
            maxIndex = i;
            maxString = a[i];
        }
    }
    return maxIndex;
}

int rotateLeft(string a[], int n, int pos){
    //check if pos is invalid (out of bounds)
    if(n < 0 || pos >= n){
        return -1;
    }
    //store item at pos, since it'll be eliminated in the loop
    string item = a[pos];
    //only "rotate" elements after index pos
    for(int i = pos + 1; i < n; i++){
        a[i - 1] = a[i];
    }
    a[n - 1] = item;
    return pos;
}

int countRuns(const string a[], int n){
    if(n < 0){
        return -1;
    }
    if(n == 0){
        return 0;
    }
    string prevString = a[0];
    //no matter what's in the array, it'll have at least 1 "run" (1st element)
    int count = 1;
    for(int i = 1; i < n; i++){
        //update run count if current element isn't equal to prev element
        if(a[i] != prevString){
            prevString = a[i];
            count++;
        }
    }
    return count;
}

int flip(string a[], int n){
    if(n < 0){
        return -1;
    }
    //swap elements of opposite halves of array
    for(int i = 0; i < n/2; i++){
        string temp = a[i];
        a[i] = a[n - i - 1];
        a[n - i - 1] = temp;
    }
    
    return n;
}


int differ(const string a1[], int n1, const string a2[], int n2){
    if(n1 < 0 || n2 < 0){
        return -1;
    }
    int end = 0;
    //checking up to whichever runs out first, n1 or n2
    if(n1 >= n2){
        end = n2;
    }else{
        end = n1;
    }
    int position = end;
    for(int i = 0; i < end; i++){
        //as soon as they differ, return position to break out of function
        if(a1[i] != a2[i]){
            position = i;
            return position;
        }
    }
    return position;
}

int subsequence(const string a1[], int n1, const string a2[], int n2){
    //check invalid inputs
    if(n2 > n1 || n1 < 0 || n2 < 0){
        return -1;
    }
    //sequence of 0 elements still considered a subsequence
    if(n2 == 0){
        return 0;
    }
    int j = 0;
    //loop through bigger array
    for(int i = 0; i < n1; i++){
        //find position in bigger array that equals a2[0] to start the check
        if(a1[i] == a2[0] && i + n2 <= n1){
            //loop through smaller array to see if elements are equal
            for(j = 0; j < n2; j++){
                if(a1[i + j] != a2[j]){
                    break;
                }
            }
            //return initial pos in a1 if all elements in a2 are in a1 consecutively and in order
            if(j == n2){
                return i;
            }
        }
    }
    return -1;
}

int lookupAny(const string a1[], int n1, const string a2[], int n2){
    if(n1 < 0 || n2 < 0){
        return -1;
    }
    int minPosition = n1 + 1;
    //loop through elements in 2nd array
    for(int i = 0; i < n2; i++){
        //loop through elements in 1st array
        for(int j = 0; j < n1; j++){
            //check if 1st array has that element in a2
            if(a2[i] == a1[j]){
                //update minimum position of a2 element occurrence in a1
                if(j < minPosition){
                    minPosition = j;
                }
                break;
            }
        }
    }
    //if minpos never changed, that means no element in a2 is in a1
    if(minPosition == n1 + 1){
        return -1;
    }
    return minPosition;
}

int split(string a[], int n, string splitter){
    if(n < 0){
        return -1;
    }
    
    for(int i = 0; i < n; i++){
        for(int j = i + 1; j < n; j++){
            if(a[i] > a[j]){
                string temp = a[i];
                a[i] = a[j];
                a[j] = temp;
            }
        }
    }
    
    int position = n;
    for(int i = 0; i < n; i++){
        if(a[i] >= splitter){
            position = i;
            break;
        }
    }
    
    return position;
}
