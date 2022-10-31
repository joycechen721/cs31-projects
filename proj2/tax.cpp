//
//  tax.cpp
//  CS31 Project2
//
//  Created by Joyce Chen on 10/7/22.
//

#include <iostream>
#include <string>
using namespace std;

int main() {
    string name, occupation;
    int numChildren;
    double income;
    double tax, tax1, tax2, tax3;
    tax = tax1 = tax2 = tax3 = 0.0;
    
    //gathering input from the user
    cout << "Name: ";
    getline(cin, name);
    
    cout << "Taxable income: ";
    cin >> income;
    cin.ignore(10000, '\n');
    
    cout << "Occupation: ";
    getline(cin, occupation);
    
    cout << "Number of children: ";
    cin >> numChildren;
    
    cout << "---" << endl;
    
    //ensure that decimal points are strictly truncated to hundredths palce
    cout.setf(ios::fixed);
    cout.precision(2);
    
    //check for invalid inputs
    if(name == "")
        cout << "You must enter a name" << endl;
    else if(income < 0)
        cout << "The taxable income must not be negative" << endl;
    else if(occupation == "")
        cout << "You must enter an occupation" << endl;
    else if(numChildren < 0)
        cout << "The number of children must not be negative" << endl;
    
    //if the inputs are valid
    else{
        //if income is below or equal to $55000, then the tax is just 4% of income
        if(income <= 55000) tax1 = income * 0.04;
        else{
            tax1 = 55000 * 0.04;
            //check occupations for discounted tax in the $55,000 - $125,000 income bracket
            if(occupation == "nurse" || occupation == "teacher"){
                if(income <= 125000) tax2 = (income - 55000) * 0.05;
                else tax2 = 70000 * 0.05;
            }
            else{
                if(income <= 125000) tax2 = (income - 55000) * 0.07;
                else tax2 = 70000 * 0.07;
            }
            //if income is above $125000, anything over is taxed 9.3%
            if(income > 125000) tax3 = (income - 125000) * 0.093;
        }
        tax = tax1 + tax2 + tax3;
        if(income < 125000) tax -= numChildren * 200;
        //consider if tax goes below 0 from child deductions
        if(tax < 0) tax = 0.00;
        cout << name << " would pay $" << tax << endl;
    }
    return 0;
}
