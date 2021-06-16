## 3.1.0

Enhancements:

* Update to new version of GEval (Haversine and BIO-Weighted-F1 metrics added)
* Add challenge-repo end-point
* Improve generating browsable links

## 3.0.0

Breaking compatibility:

* "version" property is always a list of integers

Improvements:

* Clean up Swagger documentation
* Fix metric names to be shown
* Add the proper end-point for images

## 2.3.0

Changes:

* Switch to new style of Dockerfile
* Challenge slugs can start with a digit

## 2.2.2

Improvements:

* Improve swagger and documentation

## 2.2.1

Bug fixes:

* Logging is fully working now with Nginx
* Add git-annex to the container

## 2.2.0

Bug fixes:

* UTF-8 locale is set in the container

New features:

* A challenge can marked as competition (sorting by submitter/team is enforced then)


## 2.1.0

New features:

* A team captain can invite other members

## 2.0.0

New features:

* users can be organized into teams

Enhancements:

* add end-point for listing tags

Breaking compatibility:

* switch to a different DB schema

## 1.2.0

Enhancements:

* add "version" field for challenges
* add end-point for challenge versions
* no CSRF token is required when submitting a solution via API

## 1.1.0

Improvements

* more fields in leaderboard end-point
* adding an instant pre-check for a submission

## 1.0.0

Start CHANGELOG
