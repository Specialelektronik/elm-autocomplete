# Elm Autocomplete

This is an opionionated autocomplete component for Elm. It is developed to be used as a search input where the user types at least 3 characters and get suggestions based in the query. The user can "submit" the query or choose from the provided list of results.

It is intentionally written without much styling because we use this component in different settings. As a consumer of this package you need to implement the view for the suggestions and how a selected item should look. This package only provides a basic Html.input which you have to style yourself.

# Try it out

There are 2 examples in the `/example` folder. Navigate to `/example` and run `elm reactor` to view them.

