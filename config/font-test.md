# Header 1
## Header 2
### Header 3
#### Header 4
##### Header 5

This is a piece of text.

Formatting: _italic_, **bold**, `code`

[Link to ubolonton.org](https://ubolonton.org)

List:
- a
- b
- c
   + 1
   + 2
   + 3

Check list
- [ ] a
- [X] b
- [X] c
   + [X] d
- [-] e
   + [X] f
   + [ ] g

Table:
| Title | Summary |
| X     | Y       |

<!-- A comment -->

# Code
```clojure
; A function
(defn foo [x]
  "A doc string."
  (:bar x))
```
```elisp
(module-load 'abc)
(require 'abc)
```

```org
 This is a piece of text.
*This is a piece of text.*
_This is a piece of text._
```

```markdown
 This is a piece of text.
*This is a piece of text.*
_This is a piece of text._
```

```python
# A comment.
def foo(x):
    """A doc string."""
    return str(x)
```
