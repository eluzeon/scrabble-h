import {Component, OnInit} from '@angular/core';
import {HttpClient} from '@angular/common/http';

@Component({
  selector: 'app-home-page',
  templateUrl: './home-page.component.html',
  styleUrls: ['./home-page.component.sass']
})
export class HomePageComponent implements OnInit {

  fieldArray: [any[]] = [
    [['r', ''], ['w', ''], ['w', ''], ['b', ''], ['w', ''], ['w', ''], ['w', ''], ['r', ''], ['w', ''], ['w', ''], ['w', ''], ['b', ''], ['w', ''], ['w', ''], ['r', '']],
    [['w', ''], ['p', ''], ['w', ''], ['w', ''], ['w', ''], ['n', ''], ['w', ''], ['w', ''], ['w', ''], ['n', ''], ['w', ''], ['w', ''], ['w', ''], ['p', ''], ['w', '']],
    [['w', ''], ['w', ''], ['p', ''], ['w', ''], ['w', ''], ['w', ''], ['b', ''], ['w', ''], ['b', ''], ['w', ''], ['w', ''], ['w', ''], ['p', ''], ['w', ''], ['w', '']],
    [['b', ''], ['w', ''], ['w', ''], ['p', ''], ['w', ''], ['w', ''], ['w', ''], ['b', ''], ['w', ''], ['w', ''], ['w', ''], ['p', ''], ['w', ''], ['w', ''], ['b', '']],
    [['w', ''], ['w', ''], ['w', ''], ['w', ''], ['p', ''], ['w', ''], ['w', ''], ['w', ''], ['w', ''], ['w', ''], ['p', ''], ['w', ''], ['w', ''], ['w', ''], ['w', '']],
    [['w', ''], ['n', ''], ['w', ''], ['w', ''], ['w', ''], ['n', ''], ['w', ''], ['w', ''], ['w', ''], ['n', ''], ['w', ''], ['w', ''], ['w', ''], ['n', ''], ['w', '']],
    [['w', ''], ['w', ''], ['b', ''], ['w', ''], ['w', ''], ['w', ''], ['b', ''], ['w', ''], ['b', ''], ['w', ''], ['w', ''], ['w', ''], ['b', ''], ['w', ''], ['w', '']],
    [['r', ''], ['w', ''], ['w', ''], ['b', ''], ['w', ''], ['w', ''], ['w', ''], ['c', ''], ['w', ''], ['w', ''], ['w', ''], ['b', ''], ['w', ''], ['w', ''], ['r', '']],
    [['w', ''], ['w', ''], ['b', ''], ['w', ''], ['w', ''], ['w', ''], ['b', ''], ['w', ''], ['b', ''], ['w', ''], ['w', ''], ['w', ''], ['b', ''], ['w', ''], ['w', '']],
    [['w', ''], ['n', ''], ['w', ''], ['w', ''], ['w', ''], ['n', ''], ['w', ''], ['w', ''], ['w', ''], ['n', ''], ['w', ''], ['w', ''], ['w', ''], ['n', ''], ['w', '']],
    [['w', ''], ['w', ''], ['w', ''], ['w', ''], ['p', ''], ['w', ''], ['w', ''], ['w', ''], ['w', ''], ['w', ''], ['p', ''], ['w', ''], ['w', ''], ['w', ''], ['w', '']],
    [['b', ''], ['w', ''], ['w', ''], ['p', ''], ['w', ''], ['w', ''], ['w', ''], ['b', ''], ['w', ''], ['w', ''], ['w', ''], ['p', ''], ['w', ''], ['w', ''], ['b', '']],
    [['w', ''], ['w', ''], ['p', ''], ['w', ''], ['w', ''], ['w', ''], ['b', ''], ['w', ''], ['b', ''], ['w', ''], ['w', ''], ['w', ''], ['p', ''], ['w', ''], ['w', '']],
    [['w', ''], ['p', ''], ['w', ''], ['w', ''], ['w', ''], ['n', ''], ['w', ''], ['w', ''], ['w', ''], ['n', ''], ['w', ''], ['w', ''], ['w', ''], ['p', ''], ['w', '']],
    [['r', ''], ['w', ''], ['w', ''], ['b', ''], ['w', ''], ['w', ''], ['w', ''], ['r', ''], ['w', ''], ['w', ''], ['w', ''], ['b', ''], ['w', ''], ['w', ''], ['r', '']],
  ];

  isGameStarted = false;
  numberOfPlayers = 0;
  playerNames = [];
  playersPoints = [];

  gameNumber = 0;
  playerNumber = 0;
  playerTurnNumber = 0;

  letters = [];

  isYourTurn = false;

  selectedLetters = [];

  changesBody: { positionX: number, positionY: number, letter: string }[] = [];

  constructor(private http: HttpClient) {
  }

  ngOnInit() {
    this.http.get<any>('api/initGame').subscribe(x => {
      this.gameNumber = x.playerAndGameInfo.gameNumber;
      this.playerNumber = x.playerAndGameInfo.playerNumber;
      this.letters = x.letters;
      this.numberOfPlayers = x.playerAndGameInfo.playerNumber;
    });

    this.startListening();
  }

  startListening() {
    let whileTrue = setInterval(() => {
      this.http.get<any>(`api/checkState/${this.gameNumber}`).subscribe(x => {
        if (this.isGameStarted) {
          if (x.playerTurnNumber == this.playerNumber) {
            clearInterval(whileTrue);
            this.isYourTurn = true;
          }

          x.changes.forEach(xx=>{
            this.changeState(xx.positionX, xx.positionY, xx.letter);
          });

          this.playerTurnNumber = x.playerTurnNumber;
          this.playersPoints = x.playersPoints;
        } else if(x.isGameStarted) {
          this.initStartGame();
        }
        this.numberOfPlayers = x.numberOfPlayers;
      });
    }, 2000);
  }

  clickLetter(index) {
    if (this.isYourTurn) {
      if (this.selectedLetters.includes(index)) {
        this.selectedLetters = this.selectedLetters.filter(x => x != index);
      } else {
        this.selectedLetters.push(index);
      }
    }
  }

  changeState(posX, posY, letter) {
    this.fieldArray[posX][posY][1] = letter;
  }

  startGame() {
    this.http.post(`api/startGame/${this.gameNumber}`, null)
      .subscribe(_ => {
        this.initStartGame();
      });
  }

  initStartGame() {
    this.isGameStarted = true;
    for (let i = 1; i <= this.numberOfPlayers; i++) {
      this.playerNames.push(i);
      this.playersPoints.push(0);
    }
  }

  clickField(x, y) {
    if (this.selectedLetters.length > 0 && this.fieldArray[x][y][1] == '') {
      this.changesBody.push({
        positionX: x,
        positionY: y,
        letter: this.letters[this.selectedLetters[0]]
      });
      this.changeState(x, y, this.letters[this.selectedLetters[0]]);
      this.letters.splice(this.selectedLetters[0], 1);
      this.selectedLetters = [];
    }
  }

  changeLetters() {
    if (this.selectedLetters.length == 0) {
      alert('Choose letters first');
    } else {
      this.http.post<any>(`api/changeLetters`, {
        gameNumberForSkipTurn: this.gameNumber,
        countToChange: this.selectedLetters.length
      })
        .subscribe(x => {
          let newLetters = [];
          for (let i = 0; i < this.letters.length; i++) {
            if (!this.selectedLetters.includes(i)) {
              newLetters.push(this.letters[i]);
            }
          }
          this.letters = newLetters.concat(x.lettersResult);
          this.selectedLetters = [];
          this.startListening();
          this.isYourTurn = false;
        });
    }
  }

  endTurn() {
    this.http.post<any>('api/sendChanges', {
      allChanges: this.changesBody,
      info: {
        gameNumber: this.gameNumber,
        playerNumber: this.playerNumber
      }
    }).subscribe(x => {
      this.letters = this.letters.concat(x.lettersResult);
      this.changesBody = [];
      this.selectedLetters = [];
      this.startListening();
      this.isYourTurn = false;
    });
  }
}
