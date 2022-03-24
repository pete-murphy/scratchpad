{- 
At the beginning, the bots weren't that clever. The spam detection could
essentially be narrowed down to checking messages against several simple
criteria. For a user's stream of messages over a given time period, the spammer
could be identified if:

    More than 90 % of all messages had fewer than 5 words (here, a word is defined as a sequence of consecutive letters which is neither immediately preceded nor followed by another letter);
    More than 50 % of messages to any one user had the same content, assuming that there were at least 2 messages to that user;
    More than 50 % of all messages had the same content, assuming that there were at least 2 messages;
    More than 50 % of all messages contained at least one of the words from the given list of spamSignals (the case of the letters doesn't matter).

You are applying to the Anti-Spam Team at Kik, so you want to make sure you
understand how this basic spam detection program worked. Implement a function
that, given a stream of messages and a list of spamSignals, determines whether
it's possible that the user might be a spammer by checking against the criteria
above.
-}

spamDetection messages spamSignals = 


lessThan5Words message = length (words message) < 5

lessThan5Words message = length (words message) < 5