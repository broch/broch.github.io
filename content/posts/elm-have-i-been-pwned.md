---
title: 'Checking Password Strength in Elm, Part 2: Have I Been Pwned API'
author: Luke
date: 2018-03-05
description: Checking for compromised passwords in Elm using the "Have I been Pwned" API.
thumb: /posts/elm-have-i-been-pwned/hibp_logo.png
tags: [elm,security]
lastmod: 2021-04-18
---

In the [Part 1](/posts/elm-zxcvbn), we used the Javascript library Zxcvn to check the strength of a password locally. Now we'll extend the code to check the chosen password against the huge database maintained by [Have I been pwned?](https://haveibeenpwned.com). The code for this part is similar, but instead of decoding the result of a call to Javascript to obtain our data, we make an HTTP request and decode the response.

The full code for both parts can be found [on github](https://github.com/tekul/elm-password-check).


<div class="w-full px-10 py-10 border rounded-md shadow-lg bg-neutral-200 text-neutral-700 dark:shadow-none mx-auto sm:w-2/3">
<div id="app"></div>
</div>
<script src="/posts/elm-zxcvbn/app.js"></script>
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

## Using the Password API

The API requires that we pass the first 5 hexadecimal characters of the SHA-1 hash of our password:

```plain
GET https://api.pwnedpasswords.com/range/{first 5 hash chars}
```

and it returns a list of all the suffixes of hashes with this prefix, along with the number of times each password has been found, with a colon separator [^sha-length]. For example, if we enter the very common password "password1", the SHA-1 is `e38ad214943daad1d64c102faec29de4afe9da3d`. If we sent a request using the `curl` command:

[^sha-length]: Note that a Hex encoded SHA-1 value is 40 characters long, so the suffixes we get back are 35 characters.

```shell-session
$ curl https://api.pwnedpasswords.com/range/e38ad
...
209CE6FC85F5F7B39B1FADE957076C018B7:2
20ECFBB285A5C09DE3F6DE40C6CA9F6C894:2
20F30490A32AA3D98A3F9BE594B2CAD8A80:2
214943DAAD1D64C102FAEC29DE4AFE9DA3D:2427158
223EA20780C7ED887D68E405AA5DB5BEF5D:9
2245E23E0F38934B77332C823D186739509:3
22615754CF7175ED94BDED305D7172FFF35:2
...
```

we can pick the remaining suffix (`214943daad1d64c102faec29de4afe9da3d`) out of the response and see that this password was found almost 2.5 million times! Definitely one to avoid.

## Calculating the SHA-1

The first thing we need to be able to do is calculate SHA-1 values. Fortunately, someone has already written an [Elm package which does just that](https://package.elm-lang.org/packages/TSFoster/elm-sha1/latest/).

We write our own `sha1` function to make sure we are always using upper-case Hex values. The API used Hex encoding and thought it isn't case sensitive, the response is always upper-case and we're going to be comparing with those values.

```elm
sha1 : String -> String
sha1 s =
    SHA1.fromString s
        |> SHA1.toHex
        |> String.toUpper
```

## Making The Request

This is very simple, using the standard `elm/http` package:

```elm
getPwnedMatches : String -> Cmd Msg
getPwnedMatches password =
    Http.get
        { url = "https://api.pwnedpasswords.com/range/" ++ String.left 5 (sha1 password)
        , expect = Http.expectString PwnedResults
        }
```

The `PwnedResults` msg is called when we get a response back from the API. So we need to add this to our `Msg` type:

```elm
type Msg
    = SetPassword String
    | ZxcvbnChecked Json.Value
    | CheckPwned
    | PwnedResults (Result Http.Error String)
```

There's also a `CheckPwned` message which is received when the user clicks the button to submit the form. That's when we send the request.


## Decoding The Response

We don't use a JSON decoder here since the API returns a text response, as described above.

The code to decode the response is (slightly simplified):

```elm
pwnedCountFromResponse : String -> String -> Maybe Int
pwnedCountFromResponse password response =
    let
        suffix =
            sha1 password |> String.dropLeft 5
    in
        String.lines response
            |> List.filter (String.startsWith suffix)
            |> List.head
            |> Maybe.map (String.dropLeft 36)
            |> Maybe.andThen String.toInt
```

We find our SHA-1's suffix in the list (if it's there), drop the 36 characters up to and including the colon, and then convert the remainder to an integer. The count value is stored in the model and used to present a suitable message to the user.


# Conclusion

"Have I Been Pwned?" is a great resource for accessing information on data breaches (you can also search by your email or phone number). It can be used to complement the Zxcvbn but also has the benefit that it uses real-world data, so it is less likely to miss things that Zxcvbn might, due to language differences, keyboard layouts and so on.

You might want to prevent a user from choosing a password which is found in HIBP if they are registering an account. Even if the password only has one or two matches, in a worst case scenario, your user may be reusing a password which is tied to their account in a data breach that is publicly available. Even if a password is completely random and theoretically unbreakable when hashed properly, it's useless once it's been compromised at a company which stored everything in plain text (and there have been plenty of those).

How strict you are about password policy obviously depends on what the account is for. If you have serious security requirements then passwords alone probably aren't up to the job, no matter how strong they are, and an extra multi-factor authentication solution should be used.
