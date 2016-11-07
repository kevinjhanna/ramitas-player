# Ramitas
Ramitas is a set of written events and actions formatted in a json file
that let's you build and play interactive fiction.

Note that this project is a Ramitas Player, not an editor.

## Format

```typescript
declare type EventId = string;

declare interface Adventure {
  title: string;
  description: string;
  startingEventId: EventId;
  events: { [eventId: EventId]: Event };
}

declare interface Event {
  title: string;
  description: string;
  actions: Action[];
}

declare interface Action {
  description: string;
  outputs: EventId[];
}
```


## For devs:

Live reload:

```elm-live App.elm --output=dist/app.js --open```

## Compile:

```elm-make App.elm --output=dist/app.js```

And serve ```index.html```
