---
layout: post
title: Haskell and PDF Generation with HPDF
description: ""
category: 
tags: []
---

I recently needed to generate a pdf and thought "I can just generate html with [Blaze HTML](http://jaspervdj.be/blaze/) and the convert it with [Pandoc](https://github.com/jgm/pandoc)!".

However the pandoc library was either not as simple as I thought, I'm a bit daft, or it had a [bug](https://github.com/jgm/pandoc/issues/1153). This led me to go search [PDF libraries](http://hackage.haskell.org/packages/search?terms=pdf) on Hackage. There I came across a pdf generation library called [HPDF](http://hackage.haskell.org/package/HPDF).

I read the description and went to the most top level namespace called [Graphics.PDF](http://hackage.haskell.org/package/HPDF-1.4.6/docs/Graphics-PDF.html). Upon reading the introduction paragraph, I was excited to see that the author created an [examples package](http://hackage.haskell.org/package/HPDF-1.4.6/docs/Graphics-PDF-Documentation.html). I think this is a good idea and I will try to do the same in any Haskell libraries I may create.

However, there was the problem of the imports not being listed in the examples that took me a good 10-15 minutes to figure out. I tried to Hoogle the functions first. However it appears [Hoogle has indexed the HPDF package](http://www.haskell.org/hoogle/?hoogle=Graphics.PDF), but searching for something like PDFRect doesn't work. If I had remembered it's name, I would have remembered to [search Hayoo and it would have found everything I needed](http://holumbus.fh-wedel.de/hayoo/hayoo.html?query=Graphics.PDF). It wasn't wasted since I learned the format of the library, but it kept me from getting my task done quickly.

**Protip: Use Hayoo instead of Hoogle**

So here I am, just going through the package with examples and I just can't find the imports. I feel really dumb, but then I see something about a github for the project. I search "github HPDF" and [duckduckgo finds it for me](https://github.com/alpheccar/HPDF)

Then I navigate to [Test.hs](https://github.com/alpheccar/HPDF/blob/master/Test/test.hs) as the library author recommended and I'm greeted with a huge 662 line file. This wouldn't be a problem if the majority of the functions weren't dependent on one another. This stems from the Authors choice to write the PDF generation library in [an imperative style](http://www.reddit.com/r/haskell/comments/18n0l1/library_a_collection_of_tools_for_processing_pdf/c8g84jg). The imperative style he used does have the advantage of being faster in some (maybe all?) cases. I wonder if he could get similar performance with something like Pipes using a functional style. I'm pretty sure the API would be nicer.

At this point I just copied the test.hs file and started cutting things out. Eventually I got to this:

```Haskell

{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses #-}
import Graphics.PDF
import Graphics.PDF.Typesetting

main :: IO()
main = do
    let rect = PDFRect 0 0 600 400
    runPdf "demo.pdf" (standardDocInfo { author=toPDFString "alpheccar", compressed = False}) rect $ do
        myDocument

myContent = displayFormattedText (Rectangle (80 :+ 0) (500 :+ 300)) NormalPara Normal $ do 
   paragraph $ do
        txt $ "Your Company: "
        forceNewLine
        txt $ "State: "
        forceNewLine
        txt $ "Email: "
        forceNewLine
        txt $ "Name: "

-- myDocument :: PDF () 
myDocument = do
   page1 <- addPage Nothing
   drawWithPage page1 $ do
    strokeColor red
    setWidth 0.5
    stroke $ Rectangle 800 800
    myContent

data MyVertStyles = NormalPara
                  | CirclePara !PDFFloat

data MyParaStyles = Normal
                  | Bold

instance ComparableStyle MyParaStyles where
  isSameStyleAs Normal Normal = True
  isSameStyleAs Bold Bold = True
  isSameStyleAs _ _ = False

instance Style MyParaStyles where
    textStyle Normal = TextStyle (PDFFont Times_Roman 10) black black FillText 1.0 1.0 1.0 1.0
    textStyle Bold = TextStyle (PDFFont Times_Bold 12) black black FillText 1.0 1.0 1.0 1.0

    sentenceStyle _ = Nothing
    wordStyle _ = Nothing
    
instance ParagraphStyle MyVertStyles MyParaStyles where
    lineWidth _ w _ = w
           
instance ComparableStyle MyVertStyles where
    isSameStyleAs NormalPara NormalPara = True
    isSameStyleAs _ _ = False

```


Not exactly what I would call simple and not as elegant as Haskell typically is, but things are understandable. Now that I've figured this out I can finish the task I was working on :)
