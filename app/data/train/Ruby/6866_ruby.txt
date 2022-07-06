# JOURNAL SUBMISSION GUIDELINES CONTENT USE CASE EXPECTATIONS
Then(/^I should see the submission-guidelines main content section$/) do
  expect(@app.submissionGuidelines).to have_submissionGuidelinesMain
end

Then(/^I should see the submission-guidelines on how to submit$/) do
  expect(@app.submissionGuidelines).to have_howToSubmit
  expect(@app.submissionGuidelines.howToSubmitInstruct.map {|content| content.text.to_s}).to start_with(/Correctly format your manuscript Main manuscript Genome Biology/)
  expect(@app.submissionGuidelines).to have_howToSubmitInstruct
end

Then(/^I should see the submission-guidelines link to submit your manuscript in Editorial Manager$/) do
  expect(@app.submissionGuidelines).to have_submissionLink
end