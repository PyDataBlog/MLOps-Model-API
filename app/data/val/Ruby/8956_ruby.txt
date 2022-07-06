class Goal < ActiveRecord::Base
  belongs_to :team
  belongs_to :match
  
  validates_associated :team
  validates_associated :match
  
  validates :scorer, presence: true
  validates :match,  presence: true
  validates :team,   presence: true
  validates :minute, presence: true, numericality: {greater_than: 1}
  
  validate :match_in_progress, on: :create
  
  validate :team_in_match
  
  before_create :increment_score
  after_destroy :decrement_score
  
  private
  
  def match_in_progress
    errors[:match] << "must be 'in progress' or 'finished'" if match.state? :scheduled
  end
  
  def team_in_match
    errors[:team] << "must be #{match.local.name} or #{match.visitor.name}" unless [match.local, match.visitor].include? team
  end
  
  def increment_score
    side = team_id == match.local_id ? 'local' : 'visitor'
    match.increment! "#{side}_score"
  end
  
  def decrement_score
    side = team_id == match.local_id ? 'local' : 'visitor'
    match.decrement! "#{side}_score"
  end
end
