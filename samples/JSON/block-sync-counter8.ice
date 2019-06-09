{
  "version": "1.2",
  "package": {
    "name": "counter8",
    "version": "1.0",
    "description": "",
    "author": "",
    "image": ""
  },
  "design": {
    "board": "iCEBreaker",
    "graph": {
      "blocks": [
        {
          "id": "3fe27a7f-50fc-41d7-a2a1-ad9f796d483e",
          "type": "basic.input",
          "data": {
            "name": "clk",
            "pins": [
              {
                "index": "0",
                "name": "",
                "value": ""
              }
            ],
            "virtual": true,
            "clock": true
          },
          "position": {
            "x": 96,
            "y": 104
          }
        },
        {
          "id": "e7174473-3686-41ee-8894-3fc87bd156d0",
          "type": "basic.input",
          "data": {
            "name": "up",
            "pins": [
              {
                "index": "0",
                "name": "",
                "value": ""
              }
            ],
            "virtual": true,
            "clock": false
          },
          "position": {
            "x": 72,
            "y": 192
          }
        },
        {
          "id": "630ec8a5-a44b-42a8-9e0b-e75b4e21cc22",
          "type": "basic.output",
          "data": {
            "name": "value",
            "range": "[7:0]",
            "pins": [
              {
                "index": "7",
                "name": "",
                "value": ""
              },
              {
                "index": "6",
                "name": "",
                "value": ""
              },
              {
                "index": "5",
                "name": "",
                "value": ""
              },
              {
                "index": "4",
                "name": "",
                "value": ""
              },
              {
                "index": "3",
                "name": "",
                "value": ""
              },
              {
                "index": "2",
                "name": "",
                "value": ""
              },
              {
                "index": "1",
                "name": "",
                "value": ""
              },
              {
                "index": "0",
                "name": "",
                "value": ""
              }
            ],
            "virtual": true
          },
          "position": {
            "x": 680,
            "y": 264
          }
        },
        {
          "id": "407c5757-e32a-4624-b30a-6e0b843d378d",
          "type": "basic.input",
          "data": {
            "name": "down",
            "pins": [
              {
                "index": "0",
                "name": "",
                "value": ""
              }
            ],
            "virtual": true,
            "clock": false
          },
          "position": {
            "x": 64,
            "y": 288
          }
        },
        {
          "id": "371d2121-6c35-430b-8213-cabae1486eb1",
          "type": "basic.input",
          "data": {
            "name": "reset",
            "pins": [
              {
                "index": "0",
                "name": "",
                "value": ""
              }
            ],
            "virtual": true,
            "clock": false
          },
          "position": {
            "x": 80,
            "y": 400
          }
        },
        {
          "id": "a22dd85b-90a8-44e1-b84b-5a3e58f652d4",
          "type": "basic.code",
          "data": {
            "code": "reg[7:0] value = 0;\n\nalways @(posedge clk)\n    if (reset)\n        value <= 0;\n    else if (up && !down)\n        value <= value + 1;\n    else if (down && !up)\n        value <= value - 1;\n\n        ",
            "params": [],
            "ports": {
              "in": [
                {
                  "name": "clk"
                },
                {
                  "name": "up"
                },
                {
                  "name": "down"
                },
                {
                  "name": "reset"
                }
              ],
              "out": [
                {
                  "name": "value",
                  "range": "[7:0]",
                  "size": 8
                }
              ]
            }
          },
          "position": {
            "x": 256,
            "y": 168
          },
          "size": {
            "width": 320,
            "height": 248
          }
        }
      ],
      "wires": [
        {
          "source": {
            "block": "a22dd85b-90a8-44e1-b84b-5a3e58f652d4",
            "port": "value"
          },
          "target": {
            "block": "630ec8a5-a44b-42a8-9e0b-e75b4e21cc22",
            "port": "in"
          },
          "size": 8
        },
        {
          "source": {
            "block": "3fe27a7f-50fc-41d7-a2a1-ad9f796d483e",
            "port": "out"
          },
          "target": {
            "block": "a22dd85b-90a8-44e1-b84b-5a3e58f652d4",
            "port": "clk"
          }
        },
        {
          "source": {
            "block": "e7174473-3686-41ee-8894-3fc87bd156d0",
            "port": "out"
          },
          "target": {
            "block": "a22dd85b-90a8-44e1-b84b-5a3e58f652d4",
            "port": "up"
          }
        },
        {
          "source": {
            "block": "407c5757-e32a-4624-b30a-6e0b843d378d",
            "port": "out"
          },
          "target": {
            "block": "a22dd85b-90a8-44e1-b84b-5a3e58f652d4",
            "port": "down"
          }
        },
        {
          "source": {
            "block": "371d2121-6c35-430b-8213-cabae1486eb1",
            "port": "out"
          },
          "target": {
            "block": "a22dd85b-90a8-44e1-b84b-5a3e58f652d4",
            "port": "reset"
          }
        }
      ]
    }
  },
  "dependencies": {}
}