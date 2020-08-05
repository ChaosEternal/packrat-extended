some text
{%= scheme-value %}

2nd text
{% (display "e:")  %}
3rd text
{%foreach ((var1 var2) list1 list2) -%}
in for loop {%= var1 %} other line {%= var2 %}
{%end -%}
{%if predict-cond1? -%}
text cond1 true
{% another-expression %}
other texts
{%elif prefict-cond2? -%}
text cond2 true
{%end%}
tail
