---
title: 'Checking Password Strength in Elm, Part 1: Zxcvbn'
author: Luke
date: 2018-03-04
draft: true
thumb: https://imgs.xkcd.com/comics/password_strength.png
thumbx: /images/elm-logo.svg
summary: Using the Zxcvbn JS library in an Elm application.
tags: elm,security,javascript
updated: 2021-02-25
---

As part of a recent project, I wrote some front-end user registration code in Elm which required that new users choose a password. I used the [zxcvbn](https://github.com/dropbox/zxcvbn) library to measure password strength and this seems like a good, non-trivial example to show how to call Javascript from Elm. In Part 2, we'll look at how to add extra checks by calling the [Have I Been Pwned passwords API](https://haveibeenpwned.com/API/v2#PwnedPasswords) which maintains a database of half a billion compromised password hashes.

The full code for both parts can be found [on github]().

Zxcvbn is a password strength estimator which makes use of password frequency lists, dictionaries and common patterns. The strength of a password is based on estimating the number of guesses it would take for a password cracker to find it. It's not perfect, but is better than forcing annoying rules on users such as adding [numbers and symbols to passwords](https://www.xkcd.com/936/). _Update_: The guidelines from standards authorities like NIST are now also [recommending this approach](https://stealthbits.com/blog/nist-password-guidelines/).

## Elm Ports

From Elm, we want to be able to call a `checkPassword` function, passing the password as a string. This function is a "port", so the Elm runtime knows to call an external Javascript function as the implementation.

```
port checkPassword : String -> Cmd msg


port passwordChecked : (Json.Value -> msg) -> Sub msg

```
Zxcvbn produces a JSON value containing a score for the password and information about how the score was calculated. The second port, `passwordChecked` allows us to subscribe to the Zxcvbn response. The responses will then be received as messages through the application `update` function.


## Javascript

In Javascript we need to hook up our port implementations to Javascript code. The `checkPassword` function is just implemented as a call to the `zxcvbn` function, passing in the password. It then sends the response back through the `passwordChecked` port.

```

app.ports.checkPassword.subscribe(function(password) {
    var report = zxcvbn(password);
    app.ports.passwordChecked.send(report);
});


```

## Decoding JSON

When we receive the Zxcvbn report as a JSON value, we decode it into a matching Elm data structure to make sure it contains what we expect and to make it easier to work with:

```
type alias Zxcvbn =
    { password : String
    , guesses : Float
    , guessesLog10 : Float
    , calcTime : Int
    , crackTimesSeconds : CrackTimesSeconds
    , crackTimesDisplay : CrackTimesDisplay
    , score : Int
    , feedback : Feedback
    , matchSequence : List String
    }
```

Writing decoders in Elm is straightforward but requires quite a lot of boilerplate code. You can check the code in the github repo for details. The call to Zxcvbn returns a lot of data, most of which isn't needed for a real-world application. Even though the `Zxcvbn` Elm record type above retains most of that data it could be a lot simpler [^zxcvbn_data]. For a real application, we would probably only need the `score` and `feedback` fields to provide the strength meter and information on why particular passwords are unsuitable. The decoder would then also be a lot simpler. For a demo like this though, it's interesting to show how a password is matched by the library.

[^zxcvbn_data]: The `matchSequence` field is simplified to just include a list of matched tokens and the pattern type (dictionary, date, keyboard sequence etc.) rather than the full details for each sub-match.

## The Main module

The rest of the code is a standard Elm application. We have a simple model to contain the entered password and the decoded Zxcvbn report

```
type alias Model =
    { password : String
    , zxcvbn : Maybe Zxcvbn
    }
```

In our `update` function we need to handle messages for both password input and password check responses

```
type Msg
    = SetPassword String
    | PasswordChecked Json.Value
```

The first message constructor is handled by just setting the password value in the model. For the second we call the Zxcvbn decoder and set the `zxcvbn` field in the model The `view` function just renders an HTML input field for the password and shows the strengh information for the entered value.

It's important that we remember to include the `passwordChecked` port in our `subscriptions` function so that we receive responses from the Javascript code. These will arrive wrapped in a `PasswordChecked` message

```
subscriptions : Model -> Sub Msg
subscriptions _ =
    passwordChecked PasswordChecked
```
