module Markdown where
{-| A library for markdown parsing. This is just an Elm API built on top of the
[marked](https://github.com/chjj/marked) project which focuses on speed.

# Parsing Markdown
@docs toElement, toHtml

# Parsing with Custom Options
@docs Options, defaultOptions, toElementWith, toHtmlWith
-}

import Graphics.Element exposing (Element)
import Html exposing (Html)
import Native.Markdown


{-| Turn a markdown string into an HTML element, using the `defaultOptions`.

    bodyParagraph : Html
    bodyParagraph =
        Markdown.toHtml """

    # Changing History

    In addition to time travel, Elm Reactor lets you change history...

    """
-}
toHtml : String -> Html
toHtml string =
    Native.Markdown.toHtmlWith defaultOptions string


{-| Turn a markdown string into an HTML element, using the `defaultOptions`.

    intro : Element
    intro =
        Markdown.toElement """

    # Time Travel Made Easy

    Elm Reactor grew out of my internship working on Elm at Prezi this summer...

    """
-}
toElement : String -> Element
toElement string =
    Native.Markdown.toElementWith defaultOptions string


{-| Some parser options so you can tweak things for your particular case.

  * `githubFlavored` &mdash; overall reasonable improvements on the original
    markdown parser as described [here][gfm]. This includes stuff like [fenced
    code blocks][fenced]. There are some odd parts though, such as [tables][]
    and a setting to turn all newlines into newlines in the resulting output,
    so there are settings to turn those on or off based on your preference.

  * `sanitize` &mdash; this determines if all HTML should be escaped. If you
    are parsing user markdown or user input can somehow reach the markdown
    parser, you should almost certainly turn on sanitation. If it is just you
    writing markdown, turning sanitation off is a nice way to do some HTML
    tricks if it is needed.

  * `smartypants` &mdash; This will automatically upgrade quotes to the
    prettier versions and turn dashes into [em dashes or en dashes][dash]


[gfm]: https://help.github.com/articles/github-flavored-markdown/
[fenced]: https://help.github.com/articles/github-flavored-markdown/#fenced-code-blocks
[tables]: https://help.github.com/articles/github-flavored-markdown/#tables
[dash]: http://en.wikipedia.org/wiki/Dash
-}
type alias Options =
    { githubFlavored : Maybe { tables : Bool, breaks : Bool }
    , sanitize : Bool
    , smartypants : Bool
    }


{-| The `Options` used by the `toElement` and `toHtml` functions.

    { githubFlavored = Just { tables = False, breaks = False }
    , sanitize = False
    , smartypants = False
    }
-}
defaultOptions : Options
defaultOptions =
    { githubFlavored = Just { tables = False, breaks = False }
    , sanitize = False
    , smartypants = False
    }


{-| Maybe you want to parse user input into markdown. To stop them from adding
`<script>` tags, you can use modified parsing options.

    options : Options
    options =
        { defaultOptions | sanitize <- True }

    toMarkdown : String -> Html
    toMarkdown userInput =
        Markdown.toHtmlWith options userInput
-}
toHtmlWith : Options -> String -> Html
toHtmlWith =
    Native.Markdown.toHtmlWith


{-| Maybe you want to get prettier quotes with a simple syntax. You can use
modified parsing options.

    options : Options
    options =
        { defaultOptions | smartypants <- True }

    toSmartElement : String -> Element
    toSmartElement markdown =
        Markdown.toElementWith options markdown
-}
toElementWith : Options -> String -> Element
toElementWith =
    Native.Markdown.toElementWith

