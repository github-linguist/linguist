<ul $AttributesHTML>
    <% if $Options.Count %>
        <% loop $Options %>
            <li class="$Class">
                <input id="$ID"
                       class="checkbox"
                       name="$Name"
                       type="checkbox"
                       role="$Role"
                       value="$Value.ATT"
                       <% if $isChecked %>checked="checked"<% end_if %>
                       <% if $isDisabled %>disabled="disabled"<% end_if %>
                />
                <label class="form-label" for="$ID">$Title</label>
            </li>
        <% end_loop %>
    <% else %>
        <li><%t SilverStripe\\Forms\\CheckboxSetField_ss.NOOPTIONSAVAILABLE 'No options available' %></li>
    <% end_if %>
</ul>
<%-- This example is from https://github.com/silverstripe/silverstripe-framework/blob/6/templates/SilverStripe/Forms/CheckboxSetField.ss --%>