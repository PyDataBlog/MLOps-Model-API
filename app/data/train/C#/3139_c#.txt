using System;
using System.Collections.Generic;
using System.Data.SQLite;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DescontosFidelidade
{
    public static class DbHelper
    {
        public const string DbFileName = @".dados";
        public static string BackupAppDataPath
        {
            get
            {
                var pathBase = Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData);
                return pathBase + @"\DescontosFidelidade\Backup\";
            }
        }
        public static string BackupDocumentsPath
        {
            get
            {
                var pathBase = Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments);
                return pathBase + @"\DescontosFidelidade\Backup\";
            }
        }

        private static string _dataBasePath = null;

        public static string DataBaseLocation {
            get {
                return _dataBasePath ?? 
                    (_dataBasePath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, DbFileName));
            }
        }

        public static bool ExistsDataBase()
        {
            return File.Exists(DataBaseLocation);
        }

        public static bool CanRestoreDataBase()
        {
            var localAppData = Path.Combine(BackupAppDataPath, DbFileName);
            var localProgramFiles = Path.Combine(BackupDocumentsPath, DbFileName);
            
            if (File.Exists(localAppData))
            {
                File.Copy(localAppData, DataBaseLocation);
                File.SetAttributes(DataBaseLocation, FileAttributes.Hidden);
                return true;
            } else if (File.Exists(localProgramFiles))
            {
                File.Copy(localProgramFiles, DataBaseLocation);
                File.SetAttributes(DataBaseLocation, FileAttributes.Hidden);
                return true;
            }
            return false;
        }

        public static void CreateNewDataBase()
        {
            SQLiteConnection.CreateFile(DataBaseLocation);
            File.SetAttributes(DataBaseLocation, FileAttributes.Hidden);

            using (var cn = new SQLiteConnection("Data Source=" + DataBaseLocation))
            {
                cn.Open();
                using (var cmd = cn.CreateCommand())
                {
                    cmd.CommandText = @"create table Cliente (Codigo INTEGER, Nome TEXT, Endereco TEXT, Telefone TEXT)";
                    cmd.ExecuteNonQuery();
                }
                using (var cmd = cn.CreateCommand())
                {
                    cmd.CommandText = @"create table Produto (Codigo INTEGER, Produto TEXT, PrecoTabela REAL)";
                    cmd.ExecuteNonQuery();
                }
                using (var cmd = cn.CreateCommand())
                {
                    cmd.CommandText = @"create table Desconto (Produto INTEGER, Cliente INTEGER, PrecoVenda REAL)";
                    cmd.ExecuteNonQuery();
                }
            }
            Backup();
        }

        public static void Backup()
        {
            Directory.CreateDirectory(BackupDocumentsPath);
            Directory.CreateDirectory(BackupAppDataPath);

            File.Copy(DataBaseLocation, BackupDocumentsPath + DbFileName, true);
            File.Copy(DataBaseLocation, BackupAppDataPath + DbFileName, true);

            File.SetAttributes(BackupDocumentsPath + DbFileName, FileAttributes.Normal);
            File.SetAttributes(BackupAppDataPath + DbFileName, FileAttributes.Normal);
        }
    }
}
