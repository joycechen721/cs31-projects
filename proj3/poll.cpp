//
//  poll.cpp
//  Project3
//
//  Created by Joyce Chen on 10/15/22.
//

#include <iostream>
#include <string>
#include <cctype>
#include <cassert>
using namespace std;

bool isValidPollString(string pollData);
int countSeats(string pollData, char party, int& seatCount);
bool isValidUppercaseStateCode(string stateCode);
bool isValidPartyResult(string partyResult);

int main() {
    cout << "Testing started" << endl;
    //empty poll data string
    assert(isValidPollString("")  == true);
    //state code is 1 letter
    assert(isValidPollString("I")  == false);
    //correct state code, but no party results
    assert(isValidPollString("IL")  == true);
    //correct state code, but not uppercase
    assert(isValidPollString("iL")  == true);
    //poll data that doesn't include party codes
    assert(isValidPollString("iL0")  == false);
    //single state forecast with 1 party result
    assert(isValidPollString("iL0E")  == true);
    //single state forecast with multiple party results
    assert(isValidPollString("NY9R16D1I") == true);
    //extra space after results
    assert(isValidPollString("NJ3D5R4D ")  == false);
    //extra comma with no state code following after
    assert(isValidPollString("VT,")  == false);
    //single comma, nothing before and nothing after
    assert(isValidPollString(",") == false);
    //extra space(s) within poll data string
    assert(isValidPollString("CT 0D") == false);
    //starts off with comma
    assert(isValidPollString(",VT") == false);
    //no state code before the party results & after the comma
    assert(isValidPollString("VT,3R6Y") == false);
    //poll data from 1 state with mix of single and double-digit party results
    assert(isValidPollString("VT3R06Y00D") == true);
    //party results have mix of upper and lowercase characters
    assert(isValidPollString("VT3r06Y00d") == true);
    //extra character 'I' after first poll data string
    assert(isValidPollString("NY9R16D1II,VT,NJ3D5R4D,kS4R") == false);
    //multiple state forecasts
    assert(isValidPollString("nY9R16D1I,VT,NJ3D5R4D,kS4R") == true);
    //multiple state forecasts with hyphen in between (negative number?)
    assert(isValidPollString("CT5D,NY9R16D1I,VT,ne-3r00D") == false);
    
    int seats = 0;
    //party code is an invalid space character
    assert(countSeats("", ' ', seats) == 2 && seats == 0);
    //party code is an invalid character & integer other than space
    assert(countSeats("", '%', seats) == 2 && seats == 0);
    assert(countSeats("", '1', seats) == 2 && seats == 0);
    //party code character is in state code
    assert(countSeats("IL", 'I', seats) == 0 && seats == 0);
    //party code character is in both state code and party results
    assert(countSeats("IL0I2I03I", 'I', seats) == 0 && seats == 5);
    seats = 0;
    //invalid poll data
    assert(countSeats(",", 'I', seats) == 1 && seats == 0);
    //no such party code within poll data
    assert(countSeats("VT3R06Y00D", 'I', seats) == 0 && seats == 0);
    //party code exists, but 0 votes
    assert(countSeats("VT3R06Y00D", 'D', seats) == 0 && seats == 0);
    //party code exists, mix of lowercase and uppercase
    assert(countSeats("CT5D,NY9R16D1I,VT,ne3r00D", 'r', seats) == 0 && seats == 12);
    //party code exists, but is part of state code
    assert(countSeats("CT5D,NY9R16D1I,VT,ne3r00D", 'C', seats) == 0 && seats == 0);
    //updating seat count, check for reset
    assert(countSeats("CT5D,NY9R16D1I,VT,ne3r03r", 'R', seats) == 0 && seats == 15);
    //multiple of the same party code in 1 state forecast
    assert(countSeats("CT5D,NY9R16D1I,VT,ne3r00D,TX03U9I2Z5U", 'U', seats) == 0 && seats == 8);
    //party code exists, but is part of both state code and party results
    assert(countSeats("CT5D,NY9R16D1I3C,VT,ne3r00D09C", 'C', seats) == 0 && seats == 12);
    seats = -999;
    //ensure that seats stays at -999 for an invalid input string
    assert(countSeats("CT5D,NY9R6D1I,VT,ne3r0%D", 'c', seats) == 1 && seats == -999);
    //seat updates to 0 if valid input but no party result
    assert(countSeats("CT5D,NY9R6D1I,VT,ne3r00D", 'c', seats) == 0 && seats == 0);
    //seat resets to 11
    assert(countSeats("CT5D,NY9R6D1I,VT,ne3r00D", 'd', seats) == 0 && seats == 11);
    cout << "Testing completed -- passed." << endl;
    
//    string pollData;
//    char partyLetter;
//    cout << "Enter poll data: ";
//    getline(cin, pollData);
//    cout << "Enter party letter: ";
//    cin >> partyLetter;
//
//    int seats;
//    seats = -999;
//    int x = countSeats(pollData, partyLetter, seats);
//    cout << x << " " << seats << " seats " << endl;
    
    return 0;
}

//returns true if the inputted string is a valid poll data
bool isValidPollString(string pollData){
    if(pollData == "") {
        return true;
    }
    //checking whether the poll data is at least 2 characters, and if the first 2 are a valid state code
    if (pollData.size() < 2 || !isValidUppercaseStateCode(pollData.substr(0, 2))){
        return false;
    }
    int length = 1;
    //parsing through the string starting at index 2
    for(int i = 2; i != pollData.size(); i+= length){
        char currChar = pollData.at(i);
        //a comma signifies that there should be a state code afterwards; if there is, increment by 3
        if(currChar == ',' && i + 2 < pollData.size()){
            if (!isValidUppercaseStateCode(pollData.substr(i + 1, 2))){
                return false;
            }
            length = 3;
        }
        //check whether the next 2 digits are a valid party result; if so, increment by 2
        else if(i + 1 < pollData.size() && isdigit(currChar) && isalpha(pollData.at(i+1))){
            length = 2;
        }
        //check whether the next 3 digits are a valid party result; if so, increment by 3
        else if(i + 2 < pollData.size() && isdigit(currChar) && isdigit(pollData.at(i+1)) && isalpha(pollData.at(i+2))){
            length = 3;
        }
        //return false if none of the above occurs
        else{
            return false;
        }
    }
    return true;
}

//returns 1 if not a valid string, 2 if party code isn't a letter, 0 if otherwise. also updates seat count.
int countSeats(string pollData, char party, int& seatCount){
    if(!isValidPollString(pollData)){
        return 1;
    }
    else if(!isalpha(party)){
        return 2;
    }
    seatCount = 0;
    //parse through the poll data string and find out where the party code is located
    for(int i = 0; i != pollData.size(); i++){
        if(pollData.at(i) == tolower(party) || pollData.at(i) == toupper(party)){
            //for a 1-digit number
            if(i - 1 >= 0 && isdigit(pollData.at(i - 1))){
                seatCount += (pollData.at(i - 1) - '0');
            }
            //for a 2-digit number
            if(i - 2 >= 0 && isdigit(pollData.at(i - 2))){
                seatCount += (10 * (pollData.at(i - 2) - '0'));
            }
        }
    }
    return 0;
}

//return true if the argument is a two-uppercase-letter state code, or false otherwise
bool isValidUppercaseStateCode(string stateCode)
{
    stateCode.at(0) = toupper(stateCode.at(0));
    stateCode.at(1) = toupper(stateCode.at(1));
    const string codes =
        "AL.AK.AZ.AR.CA.CO.CT.DE.FL.GA.HI.ID.IL.IN.IA.KS.KY."
        "LA.ME.MA.MD.MI.MN.MS.MO.MT.NE.NV.NH.NJ.NM.NY.NC.ND."
        "OH.OK.OR.PA.RI.SC.SD.TN.TX.UT.VT.VA.WA.WV.WI.WY";
    return (stateCode.size() == 2 &&
            stateCode.find('.') == string::npos &&  // no '.' in stateCode
            codes.find(stateCode) != string::npos);  // match found
}
