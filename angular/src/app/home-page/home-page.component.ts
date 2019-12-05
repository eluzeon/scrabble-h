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

  i = 0;

  gameNumber = 0;
  playerNumber = 0;

  letters = [];

  isYourTurn = false;

  constructor(private http: HttpClient) {
  }

  ngOnInit() {
    this.http.get<any>('api/initGame').subscribe(x => {
      this.gameNumber = x.gameNumber;
      this.playerNumber = x.playerNumber;
      this.letters = x.letters;
      console.log(x);
    });

    this.startListening();
  }

  startListening() {
    let whileTrue = setInterval(() => {
      this.http.get<any>(`api/checkState/${this.i}`).subscribe(x => {
        if (x.playerTurnNumber == this.playerNumber) {
          clearInterval(whileTrue);
          this.isYourTurn = true;
        }
        if (x.isStateChanged) {
          this.changeState(x.positionX, x.positionY, x.letter);
        }
        this.i++;
        console.log(x);
      });
    }, 1000);
  }

  changeState(posX, posY, letter) {
    // console.log(posX);
    // console.log(posY);
    // console.log(letter);
    console.log({...this.fieldArray});
    this.fieldArray[posX][14 - posY][1] = letter;
    console.log({...this.fieldArray});

  }

}
