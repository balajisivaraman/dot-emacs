# Book Metadata Auto-Population

## Overview

Enhance the existing book capture template to automatically fetch and populate book metadata (title, author, ISBN, publisher, pages, year, description) and cover image from Open Library API based on ISBN or book title search.

**Goal**: Make book note creation effortless and visually appealing, without building a complex book database system.

## Current Workflow

The existing book capture template in `init-notetaking.el`:

```elisp
("b" "Book Note" plain
 (file (lambda ()
         (expand-file-name
          (format "%s.org"
                  (read-string "Book title: "))
          bs/org-books-directory)))
 "#+TITLE: %^{Title}\n#+AUTHOR: %^{Author}\n#+FILETAGS: :book:\n#+DATE: %u\n\n%?"
 :empty-lines 1
 :unnarrowed t
 :hook (org-id-get-create))
```

**Pain points**:
- Manually typing title, author, and other metadata
- No cover image
- Prone to typos and inconsistent formatting

## Desired Workflow

1. User triggers book capture: `M-m c` → `b`
2. Prompt: "Enter ISBN or book title to search: "
3. If title search, show matching books to select from
4. Fetch metadata from Open Library API
5. Download cover image to `books/covers/` directory
6. Auto-populate template with all metadata
7. Open the note with cover embedded, ready for user's content

## Implementation Plan

### Phase 1: API Integration (~30 minutes)

Create helper functions to query Open Library API:

```elisp
;; In lisp/book-utils.el or added to init-notetaking.el

(require 'url)
(require 'json)

(defun bs/fetch-book-by-isbn (isbn)
  "Fetch book metadata from Open Library by ISBN.
Returns an alist with keys: title, authors, isbn, publisher, pages, year, cover-url."
  (let* ((url (format "https://openlibrary.org/isbn/%s.json" isbn))
         (buffer (url-retrieve-synchronously url t))
         (json-data (when buffer
                      (with-current-buffer buffer
                        (goto-char (point-min))
                        (re-search-forward "^$")
                        (json-read))))
         work-data)
    (when json-data
      ;; Get work details for more metadata
      (when-let ((work-key (alist-get 'key (aref (alist-get 'works json-data) 0))))
        (let* ((work-url (format "https://openlibrary.org%s.json" work-key))
               (work-buffer (url-retrieve-synchronously work-url t)))
          (when work-buffer
            (with-current-buffer work-buffer
              (goto-char (point-min))
              (re-search-forward "^$")
              (setq work-data (json-read))))))

      ;; Extract metadata
      (list
       (cons 'title (or (alist-get 'title json-data) "Unknown Title"))
       (cons 'authors (bs/format-authors (alist-get 'authors json-data)))
       (cons 'isbn isbn)
       (cons 'publisher (bs/get-first-publisher json-data))
       (cons 'pages (or (alist-get 'number_of_pages json-data) "Unknown"))
       (cons 'year (bs/extract-year json-data))
       (cons 'description (bs/extract-description work-data))
       (cons 'cover-url (format "https://covers.openlibrary.org/b/isbn/%s-L.jpg" isbn))))))

(defun bs/search-books-by-title (title)
  "Search for books by title using Open Library Search API.
Returns a list of books with basic metadata."
  (let* ((encoded-title (url-hexify-string title))
         (url (format "https://openlibrary.org/search.json?title=%s&limit=10" encoded-title))
         (buffer (url-retrieve-synchronously url t))
         (json-data (when buffer
                      (with-current-buffer buffer
                        (goto-char (point-min))
                        (re-search-forward "^$")
                        (json-read)))))
    (when json-data
      (mapcar #'bs/parse-search-result
              (alist-get 'docs json-data)))))

(defun bs/parse-search-result (doc)
  "Parse a search result document into simplified metadata."
  (list
   (cons 'title (alist-get 'title doc))
   (cons 'authors (mapconcat 'identity (alist-get 'author_name doc) ", "))
   (cons 'year (car (alist-get 'publish_year doc)))
   (cons 'isbn (car (alist-get 'isbn doc)))
   (cons 'publisher (car (alist-get 'publisher doc)))))

(defun bs/format-authors (authors-data)
  "Format authors from Open Library API response."
  (if (vectorp authors-data)
      (mapconcat
       (lambda (author)
         (let ((author-key (alist-get 'key (alist-get 'author author))))
           (when author-key
             ;; Could fetch author name, but often it's in the key
             (car (last (split-string author-key "/"))))))
       authors-data
       ", ")
    "Unknown Author"))

(defun bs/get-first-publisher (json-data)
  "Extract first publisher from book data."
  (let ((publishers (alist-get 'publishers json-data)))
    (if (vectorp publishers)
        (aref publishers 0)
      "Unknown Publisher")))

(defun bs/extract-year (json-data)
  "Extract publication year from book data."
  (or (alist-get 'publish_date json-data)
      "Unknown Year"))

(defun bs/extract-description (work-data)
  "Extract description from work data."
  (when work-data
    (let ((desc (alist-get 'description work-data)))
      (if (stringp desc)
          desc
        (alist-get 'value desc)))))

(defun bs/download-book-cover (cover-url filename)
  "Download book cover image from COVER-URL to FILENAME.
Returns the relative path to the downloaded image."
  (let* ((covers-dir (expand-file-name "covers/" bs/org-books-directory))
         (full-path (expand-file-name filename covers-dir)))
    ;; Ensure covers directory exists
    (unless (file-directory-p covers-dir)
      (make-directory covers-dir t))

    ;; Download cover
    (url-copy-file cover-url full-path t)

    ;; Return relative path for org-mode
    (concat "covers/" filename)))
```

### Phase 2: Interactive Functions (~20 minutes)

Create user-facing commands:

```elisp
(defun bs/select-book-interactively ()
  "Prompt for ISBN or title search and return selected book metadata."
  (interactive)
  (let* ((input (read-string "Enter ISBN or book title: "))
         (is-isbn (string-match-p "^[0-9-]+$" input)))
    (if is-isbn
        ;; Direct ISBN lookup
        (bs/fetch-book-by-isbn (replace-regexp-in-string "-" "" input))
      ;; Title search with selection
      (let* ((results (bs/search-books-by-title input))
             (choices (mapcar
                      (lambda (book)
                        (format "%s - %s (%s)"
                                (alist-get 'title book)
                                (alist-get 'authors book)
                                (alist-get 'year book)))
                      results))
             (selection (completing-read "Select book: " choices nil t))
             (index (cl-position selection choices :test #'string=))
             (selected-book (nth index results)))
        ;; If we have ISBN, fetch full details, otherwise use search data
        (if-let ((isbn (alist-get 'isbn selected-book)))
            (bs/fetch-book-by-isbn isbn)
          selected-book)))))

(defun bs/create-book-note-from-metadata (metadata)
  "Create a book note file from METADATA alist."
  (let* ((title (alist-get 'title metadata))
         (authors (alist-get 'authors metadata))
         (isbn (alist-get 'isbn metadata))
         (publisher (alist-get 'publisher metadata))
         (pages (alist-get 'pages metadata))
         (year (alist-get 'year metadata))
         (description (alist-get 'description metadata))
         (cover-url (alist-get 'cover-url metadata))
         ;; Sanitize filename
         (filename (concat (replace-regexp-in-string "[^a-zA-Z0-9 ]" "" title) ".org"))
         (filepath (expand-file-name filename bs/org-books-directory))
         ;; Download cover
         (cover-path (when cover-url
                      (bs/download-book-cover
                       cover-url
                       (concat isbn ".jpg")))))

    ;; Create the note
    (find-file filepath)
    (erase-buffer)
    (insert (format "#+TITLE: %s\n" title))
    (insert (format "#+AUTHOR: %s\n" authors))
    (when isbn (insert (format "#+ISBN: %s\n" isbn)))
    (when publisher (insert (format "#+PUBLISHER: %s\n" publisher)))
    (when pages (insert (format "#+PAGES: %s\n" pages)))
    (when year (insert (format "#+YEAR: %s\n" year)))
    (insert "#+FILETAGS: :book:\n")
    (insert (format "#+DATE: %s\n\n" (format-time-string "[%Y-%m-%d]")))

    ;; Insert cover image if available
    (when cover-path
      (insert (format "[[file:%s]]\n\n" cover-path)))

    ;; Insert description if available
    (when description
      (insert "* Description\n\n")
      (insert description)
      (insert "\n\n"))

    ;; Add standard sections
    (insert "* My Notes\n\n")
    (insert "* Highlights\n\n")
    (insert "* Key Takeaways\n\n")

    ;; Create ID
    (org-id-get-create)
    (save-buffer)

    ;; Position cursor in My Notes section
    (goto-char (point-min))
    (re-search-forward "^\\* My Notes" nil t)
    (forward-line 2)))

(defun bs/capture-book-with-metadata ()
  "Capture a book note with auto-populated metadata."
  (interactive)
  (let ((metadata (bs/select-book-interactively)))
    (when metadata
      (bs/create-book-note-from-metadata metadata)
      (message "Book note created with metadata!"))))
```

### Phase 3: Integration with org-capture (~10 minutes)

Update the book capture template in `init-notetaking.el`:

```elisp
;; Replace existing "b" book capture with:
("b" "Book Note (with metadata)" plain
 (function bs/capture-book-with-metadata)
 ""  ;; Empty template since function handles everything
 :immediate-finish t
 :jump-to-captured t)

;; Or add as separate capture:
("B" "Book Note (manual)" plain
 (file (lambda ()
         (expand-file-name
          (format "%s.org"
                  (read-string "Book title: "))
          bs/org-books-directory)))
 "#+TITLE: %^{Title}\n#+AUTHOR: %^{Author}\n#+FILETAGS: :book:\n#+DATE: %u\n\n%?"
 :empty-lines 1
 :unnarrowed t
 :hook (org-id-get-create))
```

### Phase 4: Keybindings (~5 minutes)

Add convenient keybinding:

```elisp
(general-define-key
 :prefix "M-m"
 "b" '(bs/capture-book-with-metadata :which-key "capture book (auto)"))
```

## Example Output

After entering ISBN or searching for "The Pragmatic Programmer":

```org
#+TITLE: The Pragmatic Programmer: Your Journey to Mastery
#+AUTHOR: David Thomas, Andrew Hunt
#+ISBN: 9780135957059
#+PUBLISHER: Addison-Wesley Professional
#+PAGES: 352
#+YEAR: 2019
#+FILETAGS: :book:
#+DATE: [2025-02-03]

[[file:covers/9780135957059.jpg]]

* Description

The Pragmatic Programmer is one of those rare tech books you'll read, re-read, and read again over the years. Whether you're new to the field or an experienced practitioner, you'll come away with fresh insights each and every time.

* My Notes

* Highlights

* Key Takeaways
```

## Technical Details

### Open Library API

**No API key required!** Free and open.

**Endpoints**:
- ISBN lookup: `https://openlibrary.org/isbn/{isbn}.json`
- Title search: `https://openlibrary.org/search.json?title={title}`
- Cover images: `https://covers.openlibrary.org/b/isbn/{isbn}-L.jpg`

**Rate limits**: Reasonable fair-use policy, no strict limits documented

### Alternative: Google Books API

If Open Library doesn't have good coverage, could also use Google Books API:
- Requires free API key from Google Cloud Console
- Better metadata coverage for recent books
- Similar implementation approach

## Implementation Considerations

1. **Error handling**: What if book not found? Fallback to manual entry
2. **Offline mode**: Cache API responses? (probably overkill)
3. **Cover image size**: Open Library has S/M/L sizes, we'll use L (large)
4. **File naming**: Sanitize book titles for valid filenames
5. **Existing files**: Check if book note already exists before creating

## Estimated Implementation Time

- **Phase 1**: API integration - 30 minutes
- **Phase 2**: Interactive functions - 20 minutes
- **Phase 3**: org-capture integration - 10 minutes
- **Phase 4**: Keybindings - 5 minutes
- **Testing & refinement**: 20-30 minutes

**Total**: ~1.5-2 hours

## Benefits

1. **Effortless book notes**: One command, fully populated note
2. **Consistent metadata**: No typos, standardized formatting
3. **Visual appeal**: Cover images make notes more engaging
4. **Searchable**: All metadata fields searchable with Deft
5. **No database overhead**: Just enhanced capture, works with existing Vulpea setup

## Future Enhancements

Once basic implementation is working:
- Support for multiple cover sources (Google Books, Amazon)
- Fetch book categories/subjects as tags
- Import reading progress from Kindle/Goodreads
- Link to book reviews or online resources

## Notes

This enhancement:
- ✅ Works with existing Vulpea workflow
- ✅ Creates individual book note files (not a database)
- ✅ Uses free, open APIs
- ✅ Adds visual appeal with cover images
- ✅ No complex dependencies
- ✅ Falls back gracefully if API fails
