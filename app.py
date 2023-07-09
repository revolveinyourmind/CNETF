
# 載入套件
from __future__ import unicode_literals
import os
from flask import Flask, request, abort
from linebot import LineBotApi, WebhookHandler
from linebot.exceptions import InvalidSignatureError
from linebot.models import MessageEvent, TextMessage, TextSendMessage
import json
import configparser
import pygsheets

# 建立 APP
app = Flask(__name__)

# LINE 聊天機器人的基本資料
curpath = os.path.dirname(os.path.realpath(__file__))
filename = os.path.join(curpath,"config.ini")

config = configparser.ConfigParser()
config.read(filename)

line_bot_api = LineBotApi(config.get("line-bot", "channel_access_token"))
handler = WebhookHandler(config.get("line-bot", "channel_secret"))

# 接收 LINE 的資訊並回覆
@app.route("/callback", methods=["POST"])
def linebot():
    body = request.get_data(as_text=True)
    json_data = json.loads(body)
    print(json_data)
    try:
        signature = request.headers["X-Line-Signature"]
        handler.handle(body, signature)
        tk = json_data["events"][0]["replyToken"]
        msg = json_data["events"][0]["message"]["text"]
        if msg == "中國最偉大":
            gs = pygsheets.authorize(service_file="core-parsec-377012-bf54595b7c83.json")
            result = gs.open_by_url("https://docs.google.com/spreadsheets/d/1moEt9ePoBL4xw3pqY5-R-e_FROnjCbWYIaWz5bkQ7vQ/edit?usp=sharing")
            result = result[1]
            result = result.get_all_values(include_empty_rows=False, include_tailing_empty=False, returnas='cells')
            c = result.count([])
            for i in range(c):
                buylist.remove([])
            text_message = "習大大有情報名牌，還不趕快磕頭……\n\n"
            for i in range(5):
                code = result[i+1][0].value
                name = result[i+1][1].value
                sr = result[i+1][3].value
                a = result[i+1][4].value
                b = result[i+1][5].value
                text_message = text_message + code + " " + name + "\n" + "夏普值：" + sr + "\n" + "α值：" + a + "\n" + "β值：" + b + "\n\n"
            text_message = TextSendMessage(text=text_message)
        else:
            text_message = TextSendMessage(text="現在我正一個打十個，等一下再回覆你。")
        line_bot_api.reply_message(tk,text_message)
    except:
        print("error")
    return "OK"

if __name__ == "__main__":
    app.run()
