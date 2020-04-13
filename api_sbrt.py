#!/usr/bin/python3
from flask import Flask
from flask_restful import Resource, Api, reqparse
from sqlalchemy import create_engine
from flask_jsonpify import jsonify
import re
import os
import sys



db_connect = create_engine('mysql+pymysql://elattes:Elattes2020@192.168.2.7/sbrt')
#db_connect = create_engine('sqlite:////home/micael/repositorios/github/LattesExtractor/api/base_lattes.db')



app = Flask(__name__)
api = Api(app)





#kid="K8078368H0"
#idcnpq="0000002580990139"

#ident=kid
#ident=idcnpq


class Senteca(Resource):
    def get(self, doc_id):
        conn = db_connect.connect()  # connect to database
        query = conn.execute("SELECT doc_id,p_id,sentenca_id,sentenca FROM    respostas_sentencas WHERE   doc_id = %s", (doc_id,))
        result = [dict(zip(tuple(query.keys()), i)) for i in query.cursor]
        return jsonify(result)





# Insere KID na base de dados. Recebe como entrada o IDcnpq e o KID
#http://0.0.0.0:8080/identificador/0000002580990139%skid=K4458323P6
    def post(self,doc_id):
        conn = db_connect.connect()
        parser = reqparse.RequestParser()
        parser.add_argument("p_id")
        parser.add_argument("sentenca_id")
        parser.add_argument("sentenca")
        args = parser.parse_args()
        p_id = args["p_id"]
        sentenca_id = args["sentenca_id"]
        sentenca = args["sentenca"]
        query = conn.execute("INSERT INTO respostas_sentencas VALUES (%s,%s,%s,%s)", doc_id,p_id,sentenca_id,sentenca)
        return {'status': 'success'}

api.add_resource(Senteca, '/resposta/<doc_id>')
if __name__ == '__main__':
    app.run(host='0.0.0.0', port=8282)
