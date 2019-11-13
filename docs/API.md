## Scrabble API 

- Using username field as identifier


## Generic request
```
	{
		"action": string,
		"method": string,
		"data": object
	}
```


## Generic response
```
	{
		"action": string,
		"method": string,
		"success": boolean,
		"code": number,
		"data": object,
		"detail" : string?
	}
```
**Response blocks below discribe only `data` field. **

### 1. Create room
Creates room with one user (username should be unique?)
```javascript 
	{
		"action": "room",
		"method": "create",
		"data": {
			"username": string,
		}
	}
```

response:
```javascript
	{
		...
		"data": {
			"room_id": number,
			"username": string,
			"status": "WAITING"
		}
		...
	}
```

### 2. Join room
Join user to exists room

```javascript
	{
		"action": "room",
		"method": "join",
		"data": {
			"username": string,
			"room_id": number
		}
	}
```

response (received by all memebers of this room - **broadcast**)
```javascript
	{
		"data": {
			"username": string,
			"room_id": number
		}
	}
```

### 4. Start game
Start game for room

```javascript
	{
		"action": "game",
		"method": "start",
		"data": {
			"room_id": number
		}
	}
```

**broadcast** response:
```javscript
	{
		"data": {
			"room_id": number,
			"status": "STARTED"
		}
	}
```

### 5. Set word
```
	{
		"action": "word",
		"method": "set",
		"data": {
			"username": string,
			"word": string,
			"startPointIndex": number,
			"direction": "LEFT" || "DOWN"
		}
	}
```

**broadcast** response with the same data + `points`
```
	{
		"data": {
			...
			"points": number,
			...
		}
	}
```

### 6.  End the move
Ends the move 
```
	{
		"action": "move",
		"method": "end",
		"data": {
			"username": string
		}
	}
```

**broadcast** response + `points`

```
	{
		"data": {
			...
			"points": number,
			...
		}
	}
```


### 7. Turn the move
**broadcast** response only.
Determines who's turn to set the words

```
	{
		"action": "move",
		"method": "turn",
		...
		"data": {
			"nextUsername": string,
			"points": number,
			"prevUsername": string,
		}
	}
```

### 8. Game ended
**broadcast** response when game is ended

```
	{
		"action": "game",
		"method": "end",
		...
		"data": {
			"scoreTable": [
				{
					"position": number,
					"username": string,
					"points": number,
				},
				...
			]
		}
	}
```

