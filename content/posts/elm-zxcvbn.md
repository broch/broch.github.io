---
title: 'Checking Password Strength in Elm, Part 1: Zxcvbn'
author: Luke
date: 2018-03-04
thumb: https://imgs.xkcd.com/comics/password_strength.png
thumbx: /images/elm-logo.svg
description: Using the Zxcvbn Javascript library in an Elm application.
tags: [elm,security,javascript]
lastmod: 2021-02-25
---

As part of a recent Elm project, I wrote some front-end user registration code which required that new users choose a password. I used the [zxcvbn](https://github.com/dropbox/zxcvbn) library to measure password strength and this seems like a good, non-trivial example to show how to call Javascript from Elm. In [Part 2](/posts/elm-have-i-been-pwned), we'll look at how to add extra checks by calling the [Have I Been Pwned passwords API](https://haveibeenpwned.com/API/v3#PwnedPasswords) which maintains a database of half a billion compromised password hashes.

The full code for both parts can be found [on github](https://github.com/tekul/elm-password-check).

Zxcvbn is a password strength estimator which makes use of password frequency lists, dictionaries and common patterns. Note that the default version is heavily biased towards English language so it would need to be built with custom dictionaries if that's an issue [^language].

The strength of a password is based on estimating the number of guesses it would take for a password cracker to find it. It's not perfect, but is better than forcing annoying rules on users such as adding [numbers and symbols to passwords](https://www.xkcd.com/936/) which is no longer seen as good practice. Standards authorities like NIST now [recommend](https://stealthbits.com/blog/nist-password-guidelines/) disallowing simple sequences and rejecting passwords which are found in password breach databases.

[^language]: For example, it will flag up "iloveyou" as being one of the most common passwords but it doesn't have a problem with "ichliebedich". Have I Been Pwned? doesn't suffer from this issue and will tell you that the latter has been found more than 16000 times in password data breaches, so is clearly unsuitable.

<div id="elm-zxcvbn-app" class="w-full mt-10 px-10 py-10 border rounded-md shadow-lg bg-neutral-200 text-neutral-700 dark:shadow-none mx-auto sm:w-2/3">
<div id="app"></div>
</div>
<script src="app.js"></script>
<script>
    var app = Elm.Main.init({
      node: document.getElementById('app'),
      flags: {}
    });
    app.ports.checkPassword.subscribe(function(password) {
        var report = zxcvbn(password);
        app.ports.passwordChecked.send(report);
    });
</script>
<script async src="/js/zxcvbn.js"></script>


## Elm Ports

From Elm, we want to be able to call a `checkPassword` function, passing the password as a string. This function is a "port", so the Elm runtime knows to call an external Javascript function as the implementation.

```elm
port checkPassword : String -> Cmd msg

port passwordChecked : (Json.Value -> msg) -> Sub msg
```
Zxcvbn produces a JSON value containing a score for the password and information about how the score was calculated. The second port, `passwordChecked` allows us to subscribe to the Zxcvbn response. The responses will then be received as messages through the application `update` function.


## Javascript

In Javascript we need to hook up our port implementations to Javascript code. The `checkPassword` function is just implemented as a call to the `zxcvbn` function, passing in the password. It then sends the response back through the `passwordChecked` port.

```javascript
app.ports.checkPassword.subscribe(function(password) {
    var report = zxcvbn(password);
    app.ports.passwordChecked.send(report);
});
```

## Decoding JSON

When we receive the Zxcvbn report as a JSON value, we decode it into a matching Elm data structure to make sure it contains what we expect and to make it easier to work with:

```elm
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

Writing decoders in Elm is straightforward enough but requires quite a lot of boilerplate code. You can check the github repo for details. The call to Zxcvbn returns a lot of data, most of which isn't needed for a real-world application. Even though the `Zxcvbn` Elm record type above retains most of that data it could be a lot simpler [^zxcvbn_data]. For a real application, we would probably only need the `score` and `feedback` fields to provide the strength meter and information on why particular passwords are unsuitable. The decoder would then also be a lot simpler. For a demo like this though, it's interesting to show how a password is matched by the library.

[^zxcvbn_data]: The `matchSequence` field is simplified to just include a list of matched tokens and the pattern type (dictionary, date, keyboard sequence etc.) rather than the full details for each sub-match.

## The Main module

The rest of the code is a standard Elm application. We have a simple model to contain the entered password and the decoded Zxcvbn report

```elm
type alias Model =
    { password : String
    , zxcvbn : Maybe Zxcvbn
    }
```

In our `update` function we need to handle messages for both password input and password check responses

```elm
type Msg
    = SetPassword String
    | PasswordChecked Json.Value
```

The first message constructor is handled by just setting the password value in the model. For the second we call the Zxcvbn decoder and set the `zxcvbn` field in the model. The `view` function just renders an HTML input field for the password and shows the strength information for the entered value.

It's important that we remember to include the `passwordChecked` port in our `subscriptions` function so that we receive responses from the Javascript code. These will arrive wrapped in a `PasswordChecked` message

```elm
subscriptions : Model -> Sub Msg
subscriptions _ =
    passwordChecked PasswordChecked
```

## Conclusion

It's quite easy to make use of a Javascript library like Zxcvbn, getting it to do a lot of work for us, but without compromising the safety of our Elm code. Ports provide a boundary which forces us to decode the data we get from Javascript into type-safe values before using them.

In the second part, we'll look at calling an external API to get extra information on the suitability of our chosen password.
