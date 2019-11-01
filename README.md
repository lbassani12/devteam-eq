# devteam-eq

Tiny library to calculate the pairwise affinity and team equity within a developers team using their github pull requests as a way of understanding knowledge distribution.

This was made as a university project for the subject Programming Language Analysis.

## Pairwise affinity
One concrete way of interactions within a team is through *Github Pull Requests*. This library considers that an interaction between an author and each of its reviewers is a one-sided interaction:

*Author -> Reviewer1, Reveiwer2, ...*

For a given author their affinity with a reviewer is 1 (one) per each pull request the reviewer is requested to review.The sum of each interaction is what we consider the pairwise affinity.

## Team Equity
In a team that interacts pairwise, it's equity measures how homogeneous are those interactions.

We chose to calculate it as the standard deviation of every parwise affinity stat.

## Getting Started

This proyect environment is handled using [Stack](https://docs.haskellstack.org/en/stable/README/). It is fairly easy to install and use.

Once Stack is installed, just clone the repo and execute:

```
stack build
```

This command will install all the packages and dependencies needed to use the library.

You can then do:

```
stack exec devteam-eq-exe
```

to execute the tiny `Main.hs` I did.

## Using the library

Let's check this out. First of all, you have to import the library to use it:

```haskell
import TeamEquity
```

Then you can use its functions to start working. Because we are going to be making several requests to the Github API, we need to set our credentials. Otherwise Github will no longer accept our queries:

`setCredentials` takes two overloaded strings: your *username* and your *password*. Returns an `HttpOption` credentials.

```haskell
credentials = setCredentials <YourUser> <YourPassword>
```

`setRepo` takes two overloaded strings: the *repo owner* and the *repo id*. Returns a `Repository`.

```haskell
repo = setRepo "Microsoft" "TypeScript"
```

`setDate` takes two `Day`s: *from* and *to*. Returns a `DateRange` that will constrain the search for pull requests later.
```haskell
date_range = setDate (fromGregorian 2019 10 01) (fromGregorian 2019 10 21)
```

Once we have these, we can now get our team interactions:

`getRepoInteractions` is a monadic function that takes a repo, a date range and a set of credentials. Returns a list of `Issue` (a.k.a `Issues`) wrapped in the `IO` monad, because it needs to go and query the github API, fetching all the issues in that date range.

```haskell
pr1 <- getRepoInteractions repo date_range credentials
```

Why Issues? the Github API documentation says:

> Note: GitHub's REST API v3 considers every pull request an issue, but not every issue is a pull request. For this reason, "Issues" endpoints may return both issues and pull requests in the response. You can identify pull requests by the pull_request key.

No need to panick, our implementation only fetchs the issues that *really are* pull requests. But we need to keep working on them.

`generateTeamAffMap` takes `Issues` and returns a `RepoInteractions`. This is just a list of our internal representation of what a pull request is.

```haskell
let maps = generateTeamAffMap pr1
```

This is going to be the core of our computations. It is a map of maps. The inner map keys are `Developer`s and their values a simple `Int` representing the affinity that this developer has with the outer map key: another `Developer`.

As an example, let's get a peek of what our pretty printer will output:

```
("rbuckton",[("RyanCavanaugh",2),("weswigham",1)])
```

*rbuckton* is the username of our `Developer`. This is the key to the first map. The value assosiated to it is the list that represents the second map: a list of tuples contaning other `Developer`s (i.e *RyanCavanaugh*) and the affinity they have with *rbuckton*.

Following this thought, we have a function that can get the affinity between two people, it is:

```haskell
pair_aff = getPairAff maps "rbuckton" "RyanCavanaugh"
```

This will result in `pair_aff` being **2**.

`teamAffMapPP` just prints the whole repo affinity map. This example outputs something like this:

```
      +--("weswigham",[("DanielRosenwasser",1),("ahejlsberg",1),("rbuckton",1),("sandersn",1)])
      |
   +--("uniqueiniquity",[])
   |  |
   |  +--("typescript-bot",[])
   |
+--("timsuchanek",[])
|  |
|  |  +--("sheetalkamat",[("RyanCavanaugh",5),("andrewbranch",5),("elibarzilay",5),("orta",2),("rbuckton",5),("sandersn",2),("weswigham",5)])
|  |  |
|  +--("sandersn",[("andrewbranch",3),("elibarzilay",1),("jessetrinity",1),("orta",1),("rbuckton",4),("sheetalkamat",1),("weswigham",2)])
|     |
|     |  +--("sQVe",[])
|     |  |
|     +--("rbuckton",[("RyanCavanaugh",2),("weswigham",1)])
|        |
|        +--("pirix-gh",[])
|
("orta",[("RyanCavanaugh",1),("sandersn",1)])
|
|        +--("mrcrane",[])
|        |
|     +--("mprobst",[("weswigham",1)])
|     |  |
|     |  +--("mheiber",[])
|     |
|  +--("mattlyons0",[])
|  |  |
|  |  |  +--("joeywatts",[])
|  |  |  |
|  |  +--("elibarzilay",[("RyanCavanaugh",1),("rbuckton",1)])
|  |     |
|  |     +--|
|  |
+--("andrewbranch",[("DanielRosenwasser",1),("rbuckton",1),("sandersn",1),("weswigham",1)])
   |
   |     +--|
   |     |
   |  +--("amcasey",[("RyanCavanaugh",1),("rbuckton",1),("uniqueiniquity",1)])
   |  |  |
   |  |  +--("ajafff",[])
   |  |
   +--("ahejlsberg",[("DanielRosenwasser",1),("RyanCavanaugh",1)])
      |
      |     +--|
      |     |
      |  +--("a-tarasyuk",[])
      |  |  |
      |  |  +--("RReverser",[])
      |  |
      +--("Kingwl",[])
         |
         +--("DanielRosenwasser",[("RyanCavanaugh",1)])
```

It's just the internal tree representation of the outer map coupled with printing the list representation of the inner map. Not all that pretty but it's simple and effective.

And last but not least:

`getTeamEquity` takes a `TeamAff` (our way of calling the map of maps) and calculates the standard deviation of the affinities.

```haskell
eq = getTeamEquity maps
```

`eq` takes the value `1.4439178` in this example.


And that's pretty much it.
