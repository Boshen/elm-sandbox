Elm.Test=Elm.Test||{},Elm.Test.make=function(a){"use strict";if(a.Test=a.Test||{},a.Test.values)return a.Test.values;var b=Elm.Native,c=b.Utils.make(a),d=b.List.make(a),e=(b.Array.make(a),b.Error.make(a)),f="Test",g=Elm.Basics.make(a),h=Elm.Color.make(a),i=i||{};i.Collage=Elm.Graphics.Collage.make(a);var i=i||{};i.Element=Elm.Graphics.Element.make(a);var j=Elm.Keyboard.make(a),k=(Elm.List.make(a),Elm.Maybe.make(a),k||{});k.Json=Elm.Native.Json.make(a);var k=k||{};k.Ports=Elm.Native.Ports.make(a);var l=Elm.Signal.make(a),m=(Elm.String.make(a),Elm.Text.make(a)),n=Elm.Time.make(a),o=Elm.Window.make(a),p={},q=A2(l._op["<~"],n.inSeconds,n.fps(25)),r=l.sampleOn(q)(A4(l.lift3,F3(function(a,b,c){return{ctor:"_Tuple3",_0:a,_1:b,_2:c}}),q,j.arrows,j.space)),s=F2(function(a,b){return function(){return function(){switch(a.ctor){case"_Tuple2":return function(){var c=10*g.sin(g.degrees(180-b.angle)),e=10*g.cos(g.degrees(180-b.angle));return A3(i.Collage.collage,a._0,a._1,d.fromArray([i.Collage.move({ctor:"_Tuple2",_0:g.toFloat(a._0)/4,_1:g.toFloat(a._1)/4})(i.Collage.toForm(m.asText(d.fromArray([b.x,b.y,b.angle,b.bullet.x,b.bullet.y])))),i.Collage.move({ctor:"_Tuple2",_0:g.toFloat(a._0)/4,_1:g.toFloat(a._1)/4-30})(i.Collage.toForm(m.asText(d.fromArray([b.bullet.vx,b.bullet.vy])))),i.Collage.move({ctor:"_Tuple2",_0:b.x,_1:b.y})(A2(i.Collage.filled,h.black,A2(i.Collage.rect,20,5))),i.Collage.move({ctor:"_Tuple2",_0:b.x+e,_1:b.y+c})(i.Collage.rotate(g.degrees(90-b.angle))(A2(i.Collage.filled,h.black,A2(i.Collage.rect,2,20)))),i.Collage.move({ctor:"_Tuple2",_0:b.bullet.x+2*e,_1:b.bullet.y+2*c})(A2(i.Collage.filled,h.red,i.Collage.circle(2)))]))}()}e.Case(f,"between lines 45 and 53")}()}()}),t=F2(function(a,b){return function(){return function(){var d=g.toFloat(a.y),e=b.angle+d,f=g.toFloat(a.x);return c.replace([["x",b.x+f],["angle",A3(g.clamp,0,180,e)]],b)}()}()}),u={_:{},mv:!1,vx:0,vy:0,x:0,y:0},v={_:{},angle:90,bullet:u,x:0,y:0},w=F3(function(a,b,d){return function(){var e=g.sin(g.degrees(d.angle))+a*d.bullet.vy,f=g.cos(g.degrees(180-d.angle));return c.replace([["bullet",d.bullet.mv?c.replace([["x",d.bullet.x+3*f],["y",A2(g.max,0,d.bullet.y)+3*e],["vx",d.bullet.vx-a/2],["vy",d.bullet.vy-8*a],["mv",!0]],u):c.replace([["x",d.x],["y",d.y],["mv",b||d.bullet.mv]],u)]],d)}()}),x=function(a){return function(){switch(a.ctor){case"_Tuple3":return function(b){return t(a._1)(A2(w,a._0,a._2)(b))}}e.Case(f,"on line 42, column 27 to 63")}()},y=A2(l._op["~"],A2(l._op["<~"],s,o.dimensions),A3(l.foldp,x,v,r)),z=F5(function(a,b,c,d,e){return{_:{},mv:e,vx:c,vy:d,x:a,y:b}});return a.Test.values={_op:p,bullet:u,tank:v,stepTank:t,stepBullet:w,step:x,display:s,delta:q,input:r,main:y,Bullet:z},a.Test.values},function(){var a;a=document.getElementById("test"),Elm.embed(Elm.Test,a)}.call(this);