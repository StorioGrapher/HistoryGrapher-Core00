Scenario: Mover examples
====

## Purpose 01: Move event(spool) mover with the anchor system

### Scenario 01-01: Allocate Holiday which related with days of week or days of month

In this scenario, we change the holiday X from February 1st to first monday of February.

Actually, this is very simple problem when you think this event as a function.
We just need to change the parameter.

## Scenario 01-02: Allocate Holiday after a container event

The holiday X should start after a container event.
Then, I pile the anchor of holiday X to the container event.
When I move start timing of the container event, the holiday event should move.
In other word, the holiday marker should be marked after the container event's start timing.

## Scenario 01-03: Allocate Regular event after a container event

Allocate regular event, but do not allocate the event on a holiday.

## Scenario 01-03: Move a event which contains Regular event allocator

## Purpose 02: Evaluate values based on event

### Scenario 02-01: Evaluate monthly salary and bonus based on characters and events on month
