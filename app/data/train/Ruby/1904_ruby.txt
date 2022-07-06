class CreateJobRunCounts < ActiveRecord::Migration
  def self.up
    create_table :job_run_counts do |t|
      t.integer :counter

      t.timestamps
    end
  end

  def self.down
    drop_table :job_run_counts
  end
end
