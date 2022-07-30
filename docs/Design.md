
# How Linguist Works

Linguist uses the list of known [`languages.yml`] as well as a <br>
number of methods to try to determine a file's language or a <br>
repository's breakdown graph.

<br>

## Files

Linguist starts by going through all files in a repository, excluding <br>
the following types and taking into account any **[Overrides]**.

-   **[Generated Code]**

-   **[Vendored Code]**

-   **[Documentation]**

-   **Binary Data**

-   **Data Files**

    e.g. SQL
    
-   **Prose**

    e.g. Markdown

<br>
<br>

## Overrides

If an explicit **[Language Override]** has been used, <br>
that language will used for the matching files.

<br>
<br>

## Strategy

The language of a file is determined using the following <br>
steps - in order - whereas each step either identifies the <br>
precise language or reduces the number of likely ones.

<br>

1.  ***Vim / Emacs Modeline***

2.  ***Commonly Used Filename***

3.  ***Shell Shebang***

4.  ***File Extension***

5.  ***XML Header***

6.  ***Man Page Section***

7.  ***Heuristics***

8.  ***Naive Bayesian Classification***

<br>
<br>

## Breakdown

The result of the analysis is used to produce the language <br>
stats bar which displays the languages percentages for the <br>
files in a repository.

The percentages are calculated based on the bytes of code <br>
for each language as reported by the **[List Languages]** API.

<br>

<img
    src = 'https://user-images.githubusercontent.com/2346707/91533656-9768b300-e953-11ea-808d-994cd50e6273.png'
    width = 400
    alt = 'Language Breakdown Graph Example'
/>

<br>
<br>

## GitHub.com

When you push changes to a repository on **GitHub**, <br>
a low priority background task is enqueued to that <br>
analyzes your repository as explained above.

The results of this analysis are cached for the lifetime of your <br>
repository and are updated when the repository is updated.

As this analysis is performed by a low priority background job, <br>
it can take a while - particularly during busy periods - for your <br>
language statistics bar to reflect your changes.

<br>


<!----------------------------------------------------------------------------->

[List Languages]: https://docs.github.com/rest/reference/repos#list-repository-languages

[Language Override]: Overrides.md#using-gitattributes
[`languages.yml`]: ../lib/linguist/languages.yml
[Generated Code]: Overrides.md#generated-code
[Vendored Code]: Overrides.md#vendored-code    
[Documentation]: Overrides.md#documentation
[Overrides]: Overrides.md
