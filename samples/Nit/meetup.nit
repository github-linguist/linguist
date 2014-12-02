# This file is part of NIT ( http://www.nitlanguage.org ).
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License

# Shows a meetup and allows to modify its participants
module meetup

import opportunity_model
import boilerplate
import welcome
import template

# Shows a meetup and allows to modify its participants
class OpportunityMeetupPage
	super OpportunityPage

	# Meetup the page is supposed to show
	var meetup: nullable Meetup = null
	# Answer mode for the meetup
	var mode = 0

	init from_id(id: String) do
		var db = new OpportunityDB.open("opportunity")
		meetup = db.find_meetup_by_id(id)
		db.close
		if meetup != null then mode = meetup.answer_mode
		init
	end

	init do
		header.page_js = "mode = {mode};\n"
		header.page_js += """
		function update_scores(){
			var anss = $('.answer');
			var count = {};
			var scores = {};
			var answers = [];
			var maxscore = 0;
			for(i=0; i < anss.length; i++){
				var incscore = 0;
				var inccount = 0;
				var idparts = anss[i].id.split("_");
				var ansid = idparts[1];
				var html = anss[i].innerHTML;
				if(html === "<center>✔</center>"){
					inccount = 1;
					incscore = 2;
				}else if(html === "<center>❓</center>"){
					incscore = 1;
				}
				var intansid = parseInt(ansid)
				if(answers.indexOf(intansid) == -1){
					answers.push(intansid);
				}
				if(ansid in count){
					count[ansid] += inccount;
				}else{
					count[ansid] = inccount;
				}
				if(ansid in scores){
					scores[ansid] += incscore;
				}else{
					scores[ansid] = incscore;
				}
				if(scores[ansid] > maxscore){
					maxscore = scores[ansid];
				}
			}
			for(i=0; i < answers.length; i++){
				var ansid = answers[i].toString();
				var el = $('#total'+ansid)[0];
				var ins = "<center>"+count[ansid];
				if(scores[ansid] >= maxscore){
					ins += "<br/><span style=\\"color:blue\\">★</span>";
				}
				ins += "</center>";
				el.innerHTML = ins;
			}
		}
		function change_answer(ele, id){
			// modify only the currently selected entry
			if (in_modification_id != id) return;

			var e = document.getElementById(ele.id);
			var i = e.innerHTML;
			var ans = true;"""
		if mode == 0 then
			header.page_js += """
			if(i === "<center>✔</center>"){
				ans = 0;
				e.innerHTML = "<center>✘</center>"
				e.style.color = "red";
			}else{
				ans = 1;
				e.innerHTML = "<center>✔</center>";
				e.style.color = "green";
			}"""

		else
			header.page_js += """
			if(i === "<center>✔</center>"){
				ans = 1;
				e.innerHTML = "<center>❓</center>"
				e.style.color = "#B8860B";
			}else if(i === "<center>❓</center>"){
				ans = 0;
				e.innerHTML = "<center>✘</center>"
				e.style.color = "red";
			}else{
				ans = 2;
				e.innerHTML = "<center>✔</center>";
				e.style.color = "green";
			}"""
		end
		header.page_js += """
			var a = ele.id.split('_')
			var pid = a[1]
			var aid = a[2]
			update_scores();
			$.ajax({
				type: "POST",
				url: "./rest/answer",
				data: {
					answer_id: aid,
					pers_id: pid,
					answer: ans
				}
			});
		}
		function change_temp_answer(ele){
			var e = document.getElementById(ele.id);
			var i = e.innerHTML;"""
		if mode == 0 then
			header.page_js += """
			if(i === "<center>✔</center>"){
				e.innerHTML = "<center>✘</center>"
				e.style.color = "red";
			}else{
				e.innerHTML = "<center>✔</center>";
				e.style.color = "green";
			}
			"""
		else
			header.page_js += """
			if(i === "<center>✔</center>"){
				e.innerHTML = "<center>❓</center>";
				e.style.color = "#B8860B";
			}else if(i === "<center>❓</center>"){
				e.innerHTML = "<center>✘</center>"
				e.style.color = "red";
			}else{
				e.innerHTML = "<center>✔</center>";
				e.style.color = "green";
			}
			"""
		end
		header.page_js += """
			update_scores();
		}
		function add_part(ele){
			var e = document.getElementById(ele.id);
			var pname = document.getElementById("new_name").value;
			var arr = e.id.split("_");
			var mid = arr[1];
			var ans = $('#' + ele.id).parent().parent().parent().children(".answer");
			ansmap = {};
			for(i=0;i<ans.length;i++){
				var curr = ans.eq(i)
			"""
		if mode == 0 then
			header.page_js += """
				if(curr[0].innerHTML === "<center>✔</center>"){
					ansmap[curr.attr('id')] = 1
				}else{
					ansmap[curr.attr('id')] = 0
				}"""
		else
			header.page_js += """
				if(curr[0].innerHTML === "<center>✔</center>"){
					ansmap[curr.attr('id')] = 2
				}else if(curr[0].innerHTML === "<center>❓</center>"){
					ansmap[curr.attr('id')] = 1
				}else{
					ansmap[curr.attr('id')] = 0
				}"""
		end
		header.page_js += """
			}
			$.ajax({
				type: "POST",
				url: "./rest/meetup/new_pers",
				data: {
					meetup_id: mid,
					persname: pname,
					answers: $.param(ansmap)
				}
				})
				.done(function(data){
					location.reload();
				})
				.fail(function(data){
					//TODO: Notify of failure
				});
		}
		function remove_people(ele){
			var arr = ele.id.split("_")
			var pid = arr[1]
			$('#' + ele.id).parent().parent().parent().remove();
			update_scores();
			$.ajax({
				type: "POST",
				url: "./rest/people",
				data: {
					method: "DELETE",
					p_id: pid
				}
			});
		}
		// ID of line currently open for modification
		var in_modification_id = null;
		function modify_people(ele, id){
			if (in_modification_id != null) {
				// reset to normal values
				$('#modify_'+in_modification_id).text("Modify or delete");
				$('#modify_'+in_modification_id).attr("class", "btn btn-xs btn-warning");
				$('#line_'+in_modification_id).css("background-color", "");
				$('#delete_'+in_modification_id).css("display", "none");
			}
			if (in_modification_id != id) {
				// activate modifiable mode
				$('#modify_'+id).text("Done");
				$('#modify_'+id).attr("class", "btn btn-xs btn-success");
				$('#line_'+id).css("background-color", "LightYellow");
				$('#delete_'+id).show();

				in_modification_id = id;
			} else {
				in_modification_id = null;
			}
		}
		"""
	end

	redef fun rendering do
		if meetup == null then
			add((new OpportunityHomePage).write_to_string)
			return
		end
		add header
		var db = new OpportunityDB.open("opportunity")
		add meetup.to_html(db)
		db.close
		add footer
	end
end

redef class Meetup
	# Build the HTML for `self`
	fun to_html(db: OpportunityDB): Streamable do
		var t = new Template
		t.add """
<div class="container">
<div class="page-header">
	<center><h1>{{{name}}}</h1></center>
"""
		if not date.is_empty then t.add """
	<center><h4>When: {{{date}}}</h4></center>"""

		if not place.is_empty then t.add """
	<center><h4>Where: {{{place}}}</h4></center>"""

		t.add """
</div>
<table class="table">
"""
		t.add "<th>Participant name</th>"
		for i in answers(db) do
			t.add "<th class=\"text-center\">"
			t.add i.to_s
			t.add "</th>"
		end
		t.add "<th></th>"
		t.add "</tr>"
		for i in participants(db) do
			i.load_answers(db, self)
			t.add "<tr id=\"line_{i.id}\">"
			t.add "<td>"
			t.add i.to_s
			t.add "</td>"
			for j, k in i.answers do
				var color
				if answer_mode == 0 then
					if k == 1 then
						color = "green"
					else
						color = "red"
					end
				else
					if k == 2 then
						color = "green"
					else if k == 1 then
						color = "#B8860B"
					else
						color = "red"
					end
				end
				t.add """<td class="answer" onclick="change_answer(this, {{{i.id}}})" id="answer_{{{j.id}}}_{{{i.id}}}" style="color:{{{color}}}">"""
				t.add "<center>"
				if answer_mode == 0 then
					if k == 1 then
						t.add "✔"
					else
						t.add "✘"
					end
				else
					if k == 2 then
						t.add "✔"
					else if k == 1 then
						t.add "❓"
					else
						t.add "✘"
					end
				end
				t.add "</center></td>"
			end
			t.add """<td class="opportunity-action"><center><button class="btn btn-xs btn-warning" type="button" onclick="modify_people(this, {{{i.id}}})" id="modify_{{{i.id}}}">Modify or delete</button>&nbsp;"""
			t.add """<button class="btn btn-xs btn-danger" type="button" onclick="remove_people(this)" id="delete_{{{i.id}}}" style="display: none;">Delete</button></center></td>"""
			t.add "</tr>"
		end
		t.add """
<tr id="newrow" style="background-color: LightYellow">
	<td><input id="new_name" type="text" placeholder="Your name" class="input-large"></td>
		"""
		for i in answers(db) do
			t.add "<td class=\"answer\" id=\"newans_{i.id}\" onclick=\"change_temp_answer(this)\" style=\"color:red;\"><center>✘</center></td>"
		end
		t.add """
	<td><center><span id="add_{{{id}}}" onclick="add_part(this)" style="color:green;" class="action"><button class="btn btn-xs btn-success" type="button">Done</button></span></center></td>"""
		t.add "</tr>"
		# Compute score for each answer
		var scores = new HashMap[Int, Int]
		var maxsc = 0
		for i in answers(db) do
			scores[i.id] = i.score(db)
			if scores[i.id] > maxsc then maxsc = scores[i.id]
		end
		t.add """
<tr id="total">
	<th>Total</th>
		"""
		for i in answers(db) do
			t.add """<th id="total{{{i.id}}}"><center>{{{i.count(db)}}}"""
			if scores.has_key(i.id) and scores[i.id] >= maxsc then
				t.add """<br/><span style="color:blue">★</span>"""
			end
			t.add "</center></th>"
		end
		t.add "</th>"
		t.add """
		<th></th>
</tr>"""
		t.add "</table>"
		t.add "</div>"
		return t
	end
end
