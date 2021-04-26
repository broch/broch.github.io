---
title: Double-Slit Diffraction in Elm
author: Luke
date: 2017-01-02
tags: [elm, physics]
thumb: /posts/double-slits-elm/slits.png
description: A demo of the double-slits experiment (Young's slits) written in Elm.
---

I've been meaning to try [Elm](http://elm-lang.org) for some time and finally made the effort to write some code. It's a demo of the [double-slits experiment](https://en.wikipedia.org/wiki/Double-slit_experiment). I already had some Java code which I wrote ages ago, but I've never got round to converting it to Javascript [^swatt]. Elm seems like a nice alternative so I decided to give it a try.

[^swatt]: When I was a student, my supervisor Sandy Watt wrote physics demo programs including one like this, and my Java code was based on his original version. His programs were written in Basic with embedded ARM code, but these days we can get away with a high-level language like Elm and rendering images with SVG without having to worry about performance.

<div style="margin: 0 auto; width: 750px; height: 500px;">
<div id="slits"></div>
</div>
<div id="controls" class="mt-2">
<label for="numSlits">Number of slits</label>
<select id="numSlits" onchange="changeSlits()">
        <option value="1">1</option>
        <option value="2" selected>2</option>
        <option value="3">3</option>
        <option value="4">4</option>
        <option value="5">5</option>
        <option value="6">6</option>
</select>
</div>
<script src="slits.js"></script>
<script>
    var app = Elm.Main.init({
      node: document.getElementById('slits'),
      flags: { width: 750, height: 500 }
    });
    var nSlitsNode = document.getElementById('numSlits');
    nSlitsNode.options[1].selected = true;
    function changeSlits() {
        app.ports.numberOfSlits.send(Number(nSlitsNode.value));
    }
</script>

The app shows the light source, slits and the screen with an intensity graph of the diffraction pattern. An enlarged representation of the slits is shown on the left. Slits can be dragged about or resized and the diffraction pattern updates accordingly. You can also change the number of slits, making this a rather poorly named article, but the double-slit case is the most famous.

The code is [on github](https://github.com/tekul/elm-slits). Most of it is pretty easy to follow if you've read the standard [introduction to elm](https://guide.elm-lang.org/). It also includes some use of SVG and drag and drop and the use of ports to interact with the outside.

## Overview

The most important part of the model is the [list of slits](https://github.com/tekul/elm-slits/blob/4232544c5f5c74734ada4d81667b788c33044c7f/src/Model.elm#L27) which provides the zoomed-in view of the slit array. A slit is represented by the y coordinates of its edges. Changes to the slits cause updates both in their representation in the UI and in the diffraction pattern which is rendered. The model also contains a representation of the current drag state. This is similar to the standard [elm drag and drop example](http://elm-lang.org/examples/drag) but with a few changes to deal with the differences between the page and SVG coordinate systems. It contains the starting y-coordinate, the slit being dragged and also a `DragType` value. This captures the fact that a slit can be either moved or resized by dragging it, depending on whether you click nearer the middle or the edge. The coordinates of the slit array and the screen onto which the diffraction pattern is projected are also stored but are just fixed values.

```elm
type Slit = Slit Int Int

type Drag = Drag DragType Int Slit

type alias Model =
    { slits : List Slit
    , slitsXY : (Int, Int)
    , screen : Screen
    , drag : Maybe Drag
    }

type DragType
    = WholeSlit
    | Bottom
    | Top

type Msg
    = DragStart Int Int
    | DragAt    Int
    | DragEnd
    | NumSlits Int
```

### Dealing with Coordinates

Since the slits are dragged vertically, we only need to worry about the y-coordinate of mouse events. However, we need the coordinate relative to the SVG element whereas Elm's mouse events are relative to the origin of the page [^elm-mouse]. This is fine if the application is in the top-left of the page, but if it is embedded in another page (as above), the coordinate systems no longer coincide and we need to translate between them. To do this, I created a transparent overlay rectangle which sits over the slits display, and attached a [separate `mousedown` handler](https://github.com/tekul/elm-slits/blob/da067c50569cb45409e23ab1df5714ba38896fc1/src/View.elm#L62) which creates a `DragStart` message containing both the `pageY` and [`offsetY`](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/offsetY) values. Since the overlay rectangle covers the full height of the slits, `pageY - offsetY` gives the y-origin of the SVG element in page coordinates and that allows us to translate future mouse move events from Elm to our SVG frame of reference.

[^elm-mouse]: Elm gives us the value of the [`pageY`](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/pageY) property of the mouse event.

### Interaction with Javascript

If you look at the source for this page, you'll see that the select control for setting the number of slits isn't actually part of the Elm application (though it could easily be). I decided to experiment with using a [port](https://guide.elm-lang.org/interop/javascript.html) to update the value, like so:

```elm
port numberOfSlits : (Int -> msg) -> Sub msg
```

and I [added a subscription](https://github.com/tekul/elm-slits/blob/4232544c5f5c74734ada4d81667b788c33044c7f/src/Main.elm#L66) to `numberOfSlits NumSlits`. The app then receives messages of type `NumSlits` and it can update the model accordingly. If Javascript tries to send in something invalid (not of the expected `Int` type), then Elm won't let it in and will log an error in the console.
<a href="javascript:void(0)" onclick="app.ports.numberOfSlits.send(10)">Clicking here</a>
should change the number of slits to ten.

Elm "Flags" are also used to pass the desired width and height of the SVG into the application.


## Conclusion

This was a brief overview of my first go at playing with Elm. My Javascript knowledge is rather limited, and I haven't tried out any of its competitors so I can't really draw any conclusions on how Elm compares. It certainly compared favourable with my experience of writing even small programs in Javascript. It allows you to write typed programs, with a similar syntax to Haskell and it limits what can happen in your program, making debugging a lot easier. You can step back through all the events you've received and inspect the resulting changes in the model. Elm puts you in a cosy little world within your application, where you are completely isolated from nasty runtime errors. That worked out fine for a simple application like this one, but how well it scales up into more complicated applications, I don't know yet. The next thing on my Elm to-do list is to build something that actually talks to a server.

Any questions or suggestions, please [post them on github](https://github.com/tekul/elm-slits/issues).
