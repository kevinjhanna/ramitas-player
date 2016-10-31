type EventId = string;

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
