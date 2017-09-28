#--------------------------
#
# Control, Loops, and User-Defined Functions
# Homework 
#
#--------------------------

# This homework will ask you to work with control statements, loops, and your own functions
# to continue working on some of the problems we studied in class. The material is also
# covered in the text, in Chapters 19 and 21.

#-------------
#
# Question 1:
#  A. Create a function, called "RPS", using the rock/paper/scissors code developed in class. 
#     The code should take in two arguments, one for player1 and one for player2, and
#     return the name of the winning player.
#
#  B. Now take the function in part A, and rewrite it so that if no argument is submitted 
#     for a player, the computer randomly chooses one. This will allow you to play against
#     the computer, or the computer to play against itself.
#     (Hint: ?sample will be very helpful to you)
#

#-------------
#
# Question 2:
#  The fibonacci function we wrote in class has a bug: If n <3, the code breaks. Use if
#  statements to:
#    A. Omit any non-positive-integer values
#
#    B. Return appropriate values for if n = 1 or n = 2
#

#-------------
# Question 3:
#  A. Compounding interest can be a problem when someone holds debt, but it can also work
#     in their favor when they invest. Write a for loop that calculates the total balance after
#     10 years, if a person puts away $100/month at 3% interest. (Hint: You'll need a for loop
#     because you'll need to bring foward the previous month's balance in addition to the 
#     $100 monthly contribution
#
#  B. Convert the for loop into a function that takes in the monthly contribution and interest
#     rate as variables. What are the values for:

#-------------
#
# Question 3:
#  A. Modify the calculateMonthlyBal function into one called "calculateMonthlyInterest"
#     so that it instead returns a column of total interest paid.
#
#  B. Now modify the "calcTotalMonthlyBal" function into one called "calcTotalMonthlyInterest
#     so that it returns a the total interest paid on all loans, by month. (It should look like
#     the data frame returned by "calcTotalMonthlyBal", but with interest paid instead of 
#     principal remaining.)
#
#  C. Produce one graph of the total interest paid, with just the minimum payments, an 
#     extra $20, and an extra $100. Write a short paragraph explaining the effect that
#     these extra payments have on how much the borrower actually repays compared to just 
#     making minimum payments.
#
