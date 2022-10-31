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
