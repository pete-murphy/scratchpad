Not long ago, a spam campaign originated on some of the major social
networks, and it\'s started to affect Kik users as well. Most of the
spam comes from a limited number of highly-motivated individuals,
possibly from a single group, who constantly update their spam software.
What started off as some simple message-sending bots has now evolved
into something that requires a large team of engineers to fight against
it.

At the beginning, the bots weren\'t that clever. The spam detection
could essentially be narrowed down to checking messages against several
simple criteria. For a user\'s stream of `messages` over a given time
period, the spammer could be identified if:

-   More than `90 %` of all messages had fewer than `5` words (here, a
    *word* is defined as a sequence of consecutive letters which is
    neither immediately preceded nor followed by another letter);
-   More than `50 %` of messages to any one user had the same content,
    assuming that there were at least `2` messages to that user;
-   More than `50 %` of all messages had the same content, assuming that
    there were at least `2` messages;
-   More than `50 %` of all messages contained at least one of the words
    from the given list of `spamSignals` (the case of the letters
    doesn\'t matter).

You are applying to the Anti-Spam Team at Kik, so you want to make sure
you understand how this basic spam detection program worked. Implement a
function that, given a stream of `messages` and a list of `spamSignals`,
determines whether it\'s possible that the user might be a spammer by
checking against the criteria above.

[Example]{.markdown--header style="color:#2b3b52;font-size:1.4em"}

-   For

        messages = [["Sale today!", "2837273"],
                    ["Unique offer!", "3873827"],
                    ["Only today and only for you!", "2837273"],
                    ["Sale today!", "2837273"],
                    ["Unique offer!", "3873827"]]

    and `spamSignals = ["sale", "discount", "offer"]`, the output should
    be

        spamDetection(messages, spamSignals) = [
          "passed",
          "failed: 2837273 3873827",
          "passed",
          "failed: offer sale"
        ]

    Here are the results of the checks per criterion:

    -   `4` out of `5` (`80 %`) messages have fewer than five words,
        which is within acceptable parameters = `"passed"`;
    -   `2` out of `3` messages to user `2837273` are the same and both
        messages to user `3873827` are the same, which is a good
        indicator that they might be spam = `"failed: 2837273 3873827"`;
    -   `2` out of `5` (`40 %`) messages have the same content, which is
        within acceptable parameters = `"passed"`;
    -   `4` out of `5` (`80 %`) messages contain words from
        `spamSignals`. The two words that appear in the messages are
        `offer` and `sale` and `offer` is the lexicographically smaller
        of the two, so the output = `"failed: offer sale"`.

-   For

        messages = [["Check Codesignal out", "7284736"],
                    ["Check Codesignal out", "7462832"],
                    ["Check Codesignal out", "3625374"],
                    ["Check Codesignal out", "7264762"]]

    and `spamSignals = ["sale", "discount", "offer"]`, the output should
    be

        spamDetection(messages, spamSignals) = [
          "failed: 1/1",
          "passed",
          "failed: Check Codesignal out",
          "passed"
        ]

    Since all users in `messages` received only one message each, it\'s
    impossible to check the second criterion. The fourth criterion
    doesn\'t match: there are not any words from `spamSignals` in the
    messages. However, the first and the third criteria failed, since
    all the messages contain `4` words (`"failed: 1/1"`) and have the
    exact same content (`"failed: Check Codesignal out"`).

[Input/Output]{.markdown--header style="color:#2b3b52;font-size:1.4em"}

-   **\[execution time limit\] 4 seconds (hs)**

-   **\[input\] array.array.string messages**

    An array of messages, where each message is given in the format
    `[message, id of recipient]`.

    *Guaranteed constraints:*\
    `1 ≤ messages.length ≤ 100`,\
    `messages[i].length = 2`,\
    `1 ≤ messages[i][0].length ≤ 100`,\
    `1 ≤ int(messages[i][1]) ≤ 109`.

-   **\[input\] array.string spamSignals**

    An array of unique spam signals, where each spam signal consists of
    lowercase English letters.

    *Guaranteed constraints:*\
    `1 ≤ spamSignals.length ≤ 30`,\
    `1 ≤ spamSignals[i].length ≤ 25`.

-   **\[output\] array.string**

    An array of `4` strings containing the results of the spam checks
    per criterion. The results for each criterion should be given in the
    following format:

    -   `"passed"` if the check doesn\'t suggest that the user is a
        spammer, otherwise:
        -   for the first criterion: `"failed: <failed_ratio>"`, where
            `<failed_ratio>` is the ratio of messages with fewer than
            `5` words as a [reduced
            fraction](keyword://reduced-fraction);
        -   for the second criterion:
            `"failed: <recipient_1> <recipient_2> ..."`, where
            `<recipient_i>` is ID of the spammed user. Recipients should
            be sorted in ascending order of their IDs;
        -   for the third criterion: `"failed: <message>"`, where
            `<message>` is the message, which is equal to more than
            `50%` of all messages;
        -   for the fourth criterion:
            `"failed: <spamSignal_1> <spamSignal_2> ..."`, where
            `<spamSignal_i>` is the spam signal that appeared in at
            least one message. Spam signals should be sorted
            [lexicographically](keyword://lexicographical-order-for-strings).

**\[Haskell\] Syntax Tips**

    -- Prints help message to the console
    -- Returns a string
    helloWorld name = unsafePerformIO $ do
        print "This prints to the console when you Run Tests"
        return $ "Hello, " ++ name
:::
