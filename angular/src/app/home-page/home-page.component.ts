import {Component, OnInit} from '@angular/core';

@Component({
  selector: 'app-home-page',
  templateUrl: './home-page.component.html',
  styleUrls: ['./home-page.component.sass']
})
export class HomePageComponent implements OnInit {

  fieldArray: [string[]] = [
    ['r', 'w', 'w', 'b', 'w', 'w', 'w', 'r', 'w', 'w', 'w', 'b', 'w', 'w', 'r'],
    ['w', 'p', 'w', 'w', 'w', 'n', 'w', 'w', 'w', 'n', 'w', 'w', 'w', 'p', 'w'],
    ['w', 'w', 'p', 'w', 'w', 'w', 'b', 'w', 'b', 'w', 'w', 'w', 'p', 'w', 'w'],
    ['b', 'w', 'w', 'p', 'w', 'w', 'w', 'b', 'w', 'w', 'w', 'p', 'w', 'w', 'b'],
    ['w', 'w', 'w', 'w', 'p', 'w', 'w', 'w', 'w', 'w', 'p', 'w', 'w', 'w', 'w'],
    ['w', 'n', 'w', 'w', 'w', 'n', 'w', 'w', 'w', 'n', 'w', 'w', 'w', 'n', 'w'],
    ['w', 'w', 'b', 'w', 'w', 'w', 'b', 'w', 'b', 'w', 'w', 'w', 'b', 'w', 'w'],
    ['r', 'w', 'w', 'b', 'w', 'w', 'w', 'c', 'w', 'w', 'w', 'b', 'w', 'w', 'r'],
    ['w', 'w', 'b', 'w', 'w', 'w', 'b', 'w', 'b', 'w', 'w', 'w', 'b', 'w', 'w'],
    ['w', 'n', 'w', 'w', 'w', 'n', 'w', 'w', 'w', 'n', 'w', 'w', 'w', 'n', 'w'],
    ['w', 'w', 'w', 'w', 'p', 'w', 'w', 'w', 'w', 'w', 'p', 'w', 'w', 'w', 'w'],
    ['b', 'w', 'w', 'p', 'w', 'w', 'w', 'b', 'w', 'w', 'w', 'p', 'w', 'w', 'b'],
    ['w', 'w', 'p', 'w', 'w', 'w', 'b', 'w', 'b', 'w', 'w', 'w', 'p', 'w', 'w'],
    ['w', 'p', 'w', 'w', 'w', 'n', 'w', 'w', 'w', 'n', 'w', 'w', 'w', 'p', 'w'],
    ['r', 'w', 'w', 'b', 'w', 'w', 'w', 'r', 'w', 'w', 'w', 'b', 'w', 'w', 'r'],
  ];

  constructor() {
  }

  ngOnInit() {
  }

}
