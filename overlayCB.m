clear
face_bout ={};
this_part = '/Users/tawnytsang/Desktop/Projects/ETdataFiles/infantSibs/CB-SST Data/cleaned/new/1073m3-All-Data.xlsx';
[data, txt, raw] = xlsread(this_part,'cb','A:B');
save('/Users/tawnytsang/Desktop/Projects/ETdataFiles/infantSibs/CB-SST Data/matlab/CB/9/1039m9_cb.mat','data');


points_file = {'charlieBrown/CharlieBrown_FA.mat','charlieBrown/SS_FA.mat'};

subid = '1073m3'
toplot = 0;
% % 1 is CB 2 is SST
cond = 1
% fixDu2 = data(2:end,3);
xVals = data(:,1);
yVals = data(:,2);

offScreen = find(xVals <= 0 | xVals >=1280 | yVals <= 0 | yVals >=1024);
xVals(offScreen) = NaN; yVals(offScreen)=NaN;

% -----------------------------%
% -----------------------------%
% if you want to overlay images, use this directory %%%%%%%%
if toplot ==1
RightNow=(fix(clock));
UniqueDirName=[num2str(RightNow(1))];
for i=2:6
    UniqueDirName=[UniqueDirName '-' num2str(RightNow(i))];
end
OutputDirPath = ['~/Desktop/Image_Output/OverlayFixOutput_' UniqueDirName '/'];
if exist(OutputDirPath,'dir') ~= 7
    mkdir(OutputDirPath);
end
end
% -----------------------------%
% -----------------------------%
smooth_rad = 20;
if cond == 1
    moviesize = [704 472];
elseif cond == 2
    moviesize = [720 480];
end
framesize = [1280 1024];
adjust = [framesize - moviesize]/2;

% lens = [6000 6376]; (this is for 50 frames/sec)
lens = [7210 7657]

%     figure(1)
%     clf
%     set(gca,'Color',[0 0 0],'XTick',[],'YTick',[])
  
    
    load(points_file{cond});
	fprintf('%s\n',points_file{cond});
    
        xcheck = []; xcheck2 = [];
        ycheck = []; ycheck2 = [];
%     for t = 1:lens(cond)
      for t = 1:length(xVals)
    %       xp = data{cond}(:,t,1);
    %       yp = data{cond}(:,t,2);
        xp = xVals(t)-adjust(1);
        nonadjX= xVals(t);
        nonadjY = yVals(t);
        yp = yVals(t)-adjust(2);

        cla
        f = ceil(t/6); % convert to movie frames at 10fps
%         c = ceil(t/7638/length(points));
          c = ceil(length(points)*t/lens(cond));
        if toplot == 1;
            if cond ==1;

% %-------------CHARLIE BROWN--------------%
          im = imread(['/charlieBrown/cb/charlie ' num2str(f,'%04.0f') '.jpg']);
%-------------CHARLIE BROWN--------------%
            else

%-------------SST--------------%
          im = imread(['/charlieBrown/sst/sst ' num2str(f,'%04.0f') '.jpg']);
%-------------SST--------------%
            end

          imagesc(imresize(im,moviesize([2 1])));

          axis([[0 1280] - adjust(1) [0 1024] - adjust(2)])
%           axis([0 moviesize(1) 0 moviesize(2)])
          hold on
        end

        inside = zeros(size(data(1,1),1));
        if c > length(points)
            c=length(points);

        end

        for p = 1:length(points(c).p1s)
%           for p = 1:1204;
          x1 = points(c).p1s{p}(2) - smooth_rad;
          x2 = points(c).p2s{p}(2) + smooth_rad;
          y1 = points(c).p1s{p}(1) - smooth_rad;
          y2 = points(c).p2s{p}(1) + smooth_rad;
          
          xcheck = [xcheck x1]; xcheck2 = [xcheck2 x2];
		ycheck = [ycheck y1]; ycheck2=[ycheck2 y2];
            if toplot ==1;
          rectangle('Position',[x1 y1 x2 - x1 y2 - y1],'EdgeColor',[1 0 0]); 
         drawnow
          hold on;
        plot(xVals(t), yVals(t), 'yo', 'MarkerSize', 20, 'LineWidth', 4)

		
          set(gca,'XTick',[],'YTick',[],'Color',[0 0 0]);
          m = getframe;
           cd (OutputDirPath);
          imwrite(m.cdata,[num2str(f,'%04.0f') '.jpg'],'BitDepth',8);
            end
%             inside(:,p) = xp >= x1 & xp <= x2 & yp >= y1 & yp <= y2 & ~isnan(xp) & ~isnan(yp);
             inside(:,p) = nonadjX >= x1 & nonadjX <= x2 & nonadjY >= y1 & nonadjY <= y2 & ~isnan(nonadjX) & ~isnan(nonadjY);
        end
%         end
         face_look{1}(:,t) = double(sum(inside,2)>=1);
         face_look{1}(isnan(xVals)) = NaN;
        end

      
% end

% %% look at split halves and sections
% for c = 1:2
clear c;
c = 1;
%   subplot(1,2,c)
  halves{c} = [nanmean(face_look{c}(:,1:end/2),2) nanmean(face_look{c}(:,end/2:end),2)];
  nonan_halves = halves{c}(~isnan(sum(halves{c},2)),:);
%   corr(nonan_halves)
%   plot(nonan_halves(:,1),nonan_halves(:,2),'.')
% end
% 
% %% look at mean length of bouts of face looking
% 
% for d = 1:2
face_bout{1}(1,1)=0;
d = 1;
  for i = 1:size(face_look{d},1)
    for j = 2:size(face_look{d},2)
      if face_look{d}(i,j)==1 & face_look{d}(i,j-1)==1
        face_bout{d}(i,j) = face_bout{d}(i,j-1) + 1;
      else
        face_bout{d}(i,j) = 0;
      end
    end
  end
% end
% 
% %%  now collect them
threshold = -5; % fixations must be more than 100 ms

% for d = 1:2
  for s = 1:size(face_look{d},1)
     ds = diff(face_bout{d}(s,:),1,2);
     bouts{d}{s} = -ds(ds<threshold);
     mbouts(s,d) = nanmean(bouts{d}{s})*20;
     lbouts(s,d) = length(bouts{d}{s});
  end

% end
% 
% %% charlie brown--------------
if cond ==1;
charlie = nanmean(face_look{1},2);
on_charlie = mean(~isnan(face_look{1}),2);
% % 
% subid
ss = charlie(c)
on_ss = on_charlie(c)
ss_1st=halves{1}(c,1)
ss_2nd=halves{1}(c,2)
mbouts(c,1)
lbouts(c,1)

    
% -------------------------------%

% % % SST------------------------?
% 
elseif cond ==2;
ss = nanmean(face_look{1},2)
on_ss = mean(~isnan(face_look{1}),2)

% the part where there's just a dialogue
ss_2nd = nanmean(face_look{1}(:,4300:end,:),2)
ss_1st = nanmean(face_look{1}(:,1:4300,:),2)
% on_ss_1st = nanmean(~isnan(face_look{1}(:,1:4300,:)),2)

% on_ss_2nd = nanmean(~isnan(face_look{1}(:,4300:end,:)),2)
mbouts(c,1)
lbouts(c,1)
end
% 

%%Create a file to save output **rename test to a meaningful name
file_name = fopen('data_050616.csv','a+');
% Create headers for your file
% format of file:
% Column 1: Subject ID
% Column 2: percent Face
% Column 3: percent On Task
% Column 4: Percent Face first half--kids running)
% Column 5: Percent Face 2nd half--gordon, bert, ernie)
% Column 6: Mean Duration per Fixation
% Column 7: Number Fixations
%
% RUN THE FOLLOWING 2 LINEs ONLY ONCE TO CREATE HEADER NAMES!
% fprintf(file_name,'%12s,%8s,%8s,%8s,%8s,%8s,%8s,%8s\n',...
%      'SubjectID','StimType','Face%', 'OnTask%', '1stHalf', '2ndHalf','FixDur','nFix');
% Print data to your file:
fprintf(file_name,'%12s,%1f,%2.6f,%2.6f,%2.6f,%2.6f,%2.6f,%2.6f\n', ...
     subid,cond,ss,on_ss,ss_1st,ss_2nd,mbouts,lbouts);
 fclose(file_name);





% fprintf(fid,'%s,%2.2f,%2.2f,%2.2f,%2.2f,%2.2f,%2.2f\n',...
%     subid,charlie(c),on_charlie(c), ...
%       halves{1}(c,1),halves{1}(c,2), ...
%       mbouts(c,1),lbouts(c,1));
%   fclose(fid);
  
%   
% % fprintf(fid,'subid,cb,on.cb,ss,on.ss,ss.1st,on.ss.1st,ss.2nd,on.ss.2nd,cb.h1,cb.h2,ss.h1,ss.h2,cb.bout,ss.bout,cb.num.bouts,ss.num.bouts\n')
% 
% for c = 1:length(info)
%   name = info(c).file_name;
%   name = strrep(name,'CMD.txt','');
%   fprintf(fid,'%s,%2.2f,%2.2f,%2.2f,%2.2f,%2.2f,%2.2f,%2.2f,%2.2f,%2.2f,%2.2f,%2.2f,%2.2f,%2.2f,%2.2f,%2.2f,%2.2f\n',...
%     name,charlie(c),on_charlie(c),ss(c),on_ss(c),...
%       ss_1st(c),on_ss_1st(c),ss_2nd(c),on_ss_2nd(c),...
%       halves{1}(c,1),halves{1}(c,2),halves{2}(c,1),halves{2}(c,2),...
%       mbouts(c,1),mbouts(c,2),lbouts(c,1),lbouts(c,2));
% end
% 
% fclose(fid);
